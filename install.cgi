#!/usr/bin/perl
# ==================================================================
# FileMan - enhanced files management system
#
#   Website  : http://gossamer-threads.com/
#   Support  : http://gossamer-threads.com/scripts/support/
#   CVS Info :                          
#   Revision : $Id: install.cgi,v 1.30 2008/11/27 06:38:00 brewt Exp $
#
# Copyright (c) 2001 Gossamer Threads Inc.  All Rights Reserved.
# Redistribution in part or in whole strictly prohibited. Please
# see LICENSE file for full details.
# ==================================================================

# Automated install script. Please replace the first line with
#           #!/path/to/perl
# if the install doesn't work for you.

$| = 1;

{;
#--BEGIN Libs

BEGIN {
    $INC{"GT/AutoLoader.pm"} = "GT/AutoLoader.pm";

# ==================================================================
# Gossamer Threads Module Library - http://gossamer-threads.com/
#
# GT::AutoLoader
# Author: Jason Rhinelander
# $Id: AutoLoader.pm,v 1.13 2005/03/21 06:57:58 jagerman Exp $
#
# Copyright (c) 2004 Gossamer Threads Inc. All Rights Reserved.
# ==================================================================

package GT::AutoLoader;

use vars qw($AUTOLOAD %LOG %PACKAGES);
use strict qw/vars subs/; # no strict 'refs' - we need several soft references here.

sub import {
    shift; # Discard the package, as 'use GT::AutoLoader' calls GT::AutoLoader->import(ARGS)
    my %opts = @_;

    my $pkg = caller;
    ++$PACKAGES{$pkg};

    if ($opts{LOG} and ref $opts{LOG} eq 'CODE') {
        $LOG{$pkg} = delete $opts{LOG}; # Everything that requests a log will get one for all modules
    }

    delete $opts{NAME} if $opts{NAME} and $opts{NAME} eq 'AUTOLOAD'; # Allows "if ($opts{NAME})" later on.

    my $COMPILE;
    *{$pkg . ($opts{NAME} ? "::$opts{NAME}" : '::AUTOLOAD')} = sub {
        if ($opts{NAME} or !$AUTOLOAD) { # If they're using another name, it most likely means they are wrapping the AUTOLOAD, which means we have to check for $AUTOLOAD in their package.
            $AUTOLOAD = ${$pkg . '::AUTOLOAD'};
        }
        my ($func) = $AUTOLOAD =~ /([^:]+)$/; # How odd - we use $GT::AutoLoader::AUTOLOAD, even though this is run in some other package

        if ($COMPILE = \%{$pkg . '::COMPILE'}) {
            if (defined $COMPILE->{$func}) {
                for (keys %LOG) { $LOG{$_}->($pkg, $func, 'COMPILE') }

                _compile($COMPILE, $pkg, $func);

                $AUTOLOAD = '';

                goto &{"$pkg\::$func"};
            }
        }

        if ($opts{NEXT}) {
            my ($pack, $func) = $opts{NEXT} =~ /(?:(.+)::)?([^:]+?)$/;
            $pack ||= $pkg;
            ${$pack . '::AUTOLOAD'} = $AUTOLOAD;
            my $next = "$pack\::$func";
            $AUTOLOAD = '';
            goto &$next;
        }

# It doesn't exist in %COMPILE, which means we have to look through @ISA for another AUTOLOAD to pass this to
        if (my @inh = @{"$pkg\::ISA"}) {
            while (my $inh = shift @inh) {
                my $al = $inh . '::AUTOLOAD';
                if (defined &$al) {
                    $$al = "$pkg\::$func"; # Sets $Other::Package::AUTOLOAD
                    $AUTOLOAD = '';
                    goto &$al;
                }
                elsif (my @isa = @{$inh . '::ISA'}) {
                    unshift @inh, @isa;
                }
            }
        }

        my ($file, $line) = (caller)[1,2];
        $AUTOLOAD = '';
        die "$pkg ($$, GT::AutoLoader): Unknown method '$func' called at $file line $line.\n";
    };

    my $compile = "$pkg\::COMPILE";
    *$compile = \%$compile; # Implements "use vars qw/%COMPILE/" for you

    1;
}

BEGIN {
    if ($^C) {
        eval <<'CHECK';
sub CHECK {
# ------------------------------------------------------------------------------
# In Perl 5.6+ this allows you to do: perl -cMMy::Module -e0 to make sure all
# your %COMPILE subs compile.  In versions of Perl prior to 5.6, this is simply
# treated as a sub named "CHECK", which is never called. $^C is also 5.6+
# specific - whether or not you are running under "-c"
    compile_all();
}
CHECK
    }
}

sub compile_all {
    my @pkg = @_;
    if (@pkg) {
        @pkg = grep +($PACKAGES{$_} or (warn "$_ is not loaded, does not use GT::AutoLoader, or is not a valid package" and 0)), @pkg;
        @pkg or die "No valid packages passed to compile_all()!";
    }
    else {
        @pkg = keys %PACKAGES;
    }

    for my $pkg (@pkg) {
        my $COMPILE = \%{$pkg . '::COMPILE'} or next;
        for my $func (keys %$COMPILE) {
            _compile($COMPILE, $pkg, $func) if $COMPILE->{$func};
        }
    }

    return 1;
}

sub _compile {
# ------------------------------------------------------------------------------
# Compiles a subroutine from a module's %COMPILE into the module's package.
# die()s if the subroutine cannot compile or still does not exist after
# compiling. Takes three arguments: A reference to the packages %COMPILE hash,
# the package, and the name of the function to load.
#
    my ($COMPILE, $pkg, $func) = @_;

    my $linenum = ($COMPILE->{$func} =~ s/^(\d+)//) ? $1+1 : 0;
    eval "package $pkg;\n#line $linenum$pkg\::$func\n$COMPILE->{$func}";
    if ($@) { die "Unable to load $pkg\::$func: $@" }
    if (not defined &{"$pkg\::$func"}) {
        die "Unable to load $pkg\::$func: Subroutine did not compile correctly (possible bad name).";
    }

    undef $COMPILE->{$func}; # Leave the key in the compile hash so that things can test to see if it was defined in the compile hash
    return;
}

1;


} # End of BEGIN for GT/AutoLoader.pm

BEGIN {
    $INC{"constants.pm"} = "constants.pm";

# ==================================================================
# Gossamer Threads Module Library - http://gossamer-threads.com/
#
#   constants
#   Author: Jason Rhinelander
#   CVS Info :                          
#   $Id: constants.pm,v 1.9 2004/01/13 01:35:15 jagerman Exp $
#
# Copyright (c) 2004 Gossamer Threads Inc.  All Rights Reserved.
# ==================================================================
#
# Description:
#   Lightweight version of the standard constant.pm that allows you
#   to declare multiple scalar constants in a single compile-time
#   command. Like constant.pm, these scalar constants are optimized
#   during Perl's compilation stage.
#   Unlike constant.pm, this does not allow you to declare list
#   constants.

package constants;


use strict;
use Carp;
use vars qw($VERSION);

$VERSION = '1.00';

#=======================================================================

# Some of this stuff didn't work in version 5.003, alas.
require 5.003_96;

#=======================================================================
# import() - import symbols into user's namespace
#
# What we actually do is define a function in the caller's namespace
# which returns the value. The function we create will normally
# be inlined as a constant, thereby avoiding further sub calling 
# overhead.
#=======================================================================
sub import {
    my $class = shift;
    @_ or return; # Ignore 'use constant;'
    my %constants = @_;
    my $pkg = caller;
    {
        no strict 'refs';
        for my $name (keys %constants) {
            croak qq{Can't define "$name" as constant} .
                qq{ (name contains invalid characters or is empty)}
                unless $name =~ /^[^\W_0-9]\w*$/;
            my $scalar = $constants{$name};
            *{"${pkg}::$name"} = sub () { $scalar };
        }
    }

}

1;


} # End of BEGIN for constants.pm

BEGIN {
    $INC{"GT/Base.pm"} = "GT/Base.pm";

# ==================================================================
# Gossamer Threads Module Library - http://gossamer-threads.com/
#
#   GT::Base
#   Author  : Alex Krohn
#   $Id: Base.pm,v 1.135 2007/11/10 06:46:21 brewt Exp $
#
# Copyright (c) 2004 Gossamer Threads Inc.  All Rights Reserved.
# ==================================================================
#
# Description:
#   Base module that handles common functions like initilization,
#   debugging, etc. Should not be used except as a base class.
#

package GT::Base;
# ===============================================================
require 5.004;              # We need perl 5.004 for a lot of the OO features.

use strict qw/vars subs/;   # No refs as we do some funky stuff.
use vars   qw/$AUTOLOAD $DEBUG $VERSION $ATTRIB_CACHE %ERRORS @EXPORT_OK %EXPORT_TAGS @ISA/;
use GT::AutoLoader(NEXT => 'GT::Base::_AUTOLOAD');
use Exporter();

# We need to inherit from Exporter for ->require_version support
@ISA = qw/Exporter/;

BEGIN {
    if ($ENV{MOD_PERL}) {
        eval { require mod_perl2 } or eval { require mod_perl };
    }
    require CGI::SpeedyCGI if $CGI::SpeedyCGI::i_am_speedy or $CGI::SpeedyCGI::_i_am_speedy;
}
use constants
    MOD_PERL => $ENV{MOD_PERL} ? $mod_perl2::VERSION || $mod_perl::VERSION : 0,
    SPEEDY   => $CGI::SpeedyCGI::_i_am_speedy || $CGI::SpeedyCGI::i_am_speedy ? $CGI::SpeedyCGI::VERSION : 0;
use constants
    PERSIST => MOD_PERL || SPEEDY;

$DEBUG        = 0;
$VERSION      = sprintf "%d.%03d", q$Revision: 1.135 $ =~ /(\d+)\.(\d+)/;
$ATTRIB_CACHE = {};
%ERRORS       = (
    MKDIR     => "Could not make directory '%s': %s",
    OPENDIR   => "Could not open directory '%s': %s",
    RMDIR     => "Could not remove directory '%s': %s",
    CHMOD     => "Could not chmod '%s': %s",
    UNLINK    => "Could not unlink '%s': %s",
    READOPEN  => "Could not open '%s' for reading: %s",
    WRITEOPEN => "Could not open '%s' for writing: %s",
    OPEN      => "Could not open '%s': %s",
    BADARGS   => "Wrong argument passed to this subroutine. %s"
);
@EXPORT_OK = qw/MOD_PERL SPEEDY PERSIST $MOD_PERL $SPEEDY $PERSIST/;
%EXPORT_TAGS = (
    all     => \@EXPORT_OK,
    persist => [qw/MOD_PERL SPEEDY PERSIST/]
);

# These three are for backwards-compatibility with what GT::Base used to
# export; new code should import and use the constants of the same name.
use vars qw/$MOD_PERL $SPEEDY $PERSIST/;
$MOD_PERL = MOD_PERL;
$SPEEDY   = SPEEDY;
$PERSIST  = PERSIST;

sub new {
# -------------------------------------------------------
# Create a base object and use set or init to initilize anything.
#
    my $this  = shift;
    my $class = ref $this || $this;

# Create self with our debug value.
    my $self = { _debug => defined ${"$class\:\:DEBUG"}  ? ${"$class\:\:DEBUG"} : $DEBUG };
    bless $self, $class;
    $self->debug("Created new $class object.") if $self->{_debug} > 2;

# Set initial attributes, and then run init function or call set.
    $self->reset;
    if ($self->can('init')) {
        $self->init(@_);
    }
    else {
        $self->set(@_) if (@_);
    }

    if (index($self, 'HASH') != -1) {
        $self->{_debug} = $self->{debug} if $self->{debug};
    }
    return $self;
}

sub DESTROY {
# -------------------------------------------------------
# Object is nuked.
#
    (index($_[0], 'HASH') > -1) or return;
    if ($_[0]->{_debug} and $_[0]->{_debug} > 2) {
        my ($package, $filename, $line) = caller;
        $_[0]->debug("Destroyed $_[0] in package $package at $filename line $line.");
    }
}

sub _AUTOLOAD {
# -------------------------------------------------------
# We use autoload to provide an accessor/setter for all
# attributes.
#
    my ($self, $param) = @_;
    my ($attrib)       = $AUTOLOAD =~ /::([^:]+)$/;

# If this is a known attribute, return/set it and save the function
# to speed up future calls.
    my $autoload_attrib = 0;
    if (ref $self and index($self, 'HASH') != -1 and exists $self->{$attrib} and not exists $COMPILE{$attrib}) {
        $autoload_attrib = 1;
    }
    else {
# Class method possibly.
        unless (ref $self) {
            my $attribs = $ATTRIB_CACHE->{$self} || _get_attribs($self);
            if (exists $attribs->{$attrib}) {
                $autoload_attrib = 1;
            }
        }
    }
# This is an accessor, create a function for it.
    if ($autoload_attrib) {
        *{$AUTOLOAD} = sub {
            unless (ref $_[0]) { # Class Method
                my $attribs = $ATTRIB_CACHE->{$_[0]} || _get_attribs($_[0]);
                if (@_ > 1) {
                    $_[0]->debug("Setting base attribute '$attrib' => '$_[1]'.") if defined ${$_[0] . '::DEBUG'} and ${$_[0] . '::DEBUG'} > 2;
                    $ATTRIB_CACHE->{$_[0]}->{$attrib} = $_[1];
                }
                return $ATTRIB_CACHE->{$_[0]}->{$attrib};
            }
            if (@_ > 1) { # Instance Method
                $_[0]->debug("Setting '$attrib' => '$_[1]'.") if $_[0]->{_debug} and $_[0]->{_debug} > 2;
                $_[0]->{$attrib} = $_[1];
            }
            return $_[0]->{$attrib};
        };
        goto &$AUTOLOAD;
    }

# Otherwise we have an error, let's help the user out and try to
# figure out what they were doing.
    _generate_fatal($self, $attrib, $param);
}

sub set {
# -------------------------------------------------------
# Set one or more attributes.
#
    return unless (@_);
    if   ( !ref $_[0]) { class_set(@_); }
    else {
        my $self    = shift;
        my $p       = $self->common_param(@_) or return $self->error('BADARGS', 'FATAL', "Argument to set must be either hash, hash ref, array, array ref or CGI object.");
        my $attribs = $ATTRIB_CACHE->{ref $self} || _get_attribs(ref $self);
        my $f = 0;
        $self->{_debug} = $p->{debug} || 0 if exists $p->{debug};
        foreach my $attrib (keys %$attribs) {
            next unless exists $p->{$attrib};
            $self->debug("Setting '$attrib' to '${$p}{$attrib}'.") if $self->{_debug} and $self->{_debug} > 2;
            $self->{$attrib} = $p->{$attrib};
            $f++;
        }
        return $f;
    }
}

sub common_param {
# -------------------------------------------------------
# Expects to find $self, followed by one or more arguments of
# unknown types. Converts them to hash refs.
#
    shift;
    my $out = {};
    return $out unless @_ and defined $_[0];
    CASE: {
        (ref $_[0] eq 'HASH')               and do { $out = shift; last CASE };
        (UNIVERSAL::can($_[0], 'get_hash')) and do { $out = $_[0]->get_hash; last CASE };
        (UNIVERSAL::can($_[0], 'param'))    and do { foreach ($_[0]->param) { my @vals = $_[0]->param($_); $out->{$_} = (@vals > 1) ? \@vals : $vals[0]; } last CASE };
        (defined $_[0] and not @_ % 2)      and do { $out = {@_}; last CASE };
        return;
    }
    return $out;
}

sub reset {
# -------------------------------------------------------
# Resets all attribs in $self.
#
    my $self   = shift;
    my $class  = ref $self;
    my $attrib = $ATTRIB_CACHE->{$class} || _get_attribs($class);

# Deep copy hash and array refs only.
    while (my ($k, $v) = each %$attrib) {
        unless (ref $v) {
            $self->{$k} = $v;
        }
        elsif (ref $v eq 'HASH') {
            $self->{$k} = {};
            foreach my $k1 (keys %{$attrib->{$k}}) {
                $self->{$k}->{$k1} = $attrib->{$k}->{$k1};
            }
        }
        elsif (ref $v eq 'ARRAY') {
            $self->{$k} = [];
            foreach my $v1 (@{$attrib->{$k}}) {
                push @{$self->{$k}}, $v1;
            }
        }
        else {
            $self->{$k} = $v;
        }
    }
}

sub _get_attribs {
# -------------------------------------------------------
# Searches through ISA and returns this packages attributes.
#
    my $class   = shift;
    my $attrib  = defined ${"$class\:\:ATTRIBS"} ? ${"$class\:\:ATTRIBS"} : {};
    my @pkg_isa = defined @{"$class\:\:ISA"}     ? @{"$class\:\:ISA"}     : ();

    foreach my $pkg (@pkg_isa) {
        next if $pkg eq 'Exporter'; # Don't mess with Exporter.
        next if $pkg eq 'GT::Base';
        my $fattrib = defined ${"${pkg}::ATTRIBS"} ? ${"${pkg}::ATTRIBS"} : next;
        foreach (keys %{$fattrib}) {
            $attrib->{$_} = $fattrib->{$_} unless exists $attrib->{$_};
        }
    }
    $ATTRIB_CACHE->{$class} = $attrib;
    return $attrib;
}

$COMPILE{debug} = __LINE__ . <<'END_OF_FUNC';
sub debug {
# -------------------------------------------------------
# Displays a debugging message.
#
    my ($self, $msg) = @_;
    my $pkg = ref $self || $self;

# Add line numbers if asked for.
    if ($msg !~ /\r?\n$/) {
        my ($package, $file, $line) = caller;
        $msg .= " at $file line $line.\n";
    }
# Remove windows linefeeds (breaks unix terminals).
    $msg =~ s/\r//g unless ($^O eq 'MSWin32');
    $msg =~ s/\n(?=[^ ])/\n\t/g;
    if ($SIG{__WARN__}) {
        CORE::warn("$pkg ($$): $msg");
    }
    else {
        print STDERR "$pkg ($$): $msg";
    }
}
END_OF_FUNC

$COMPILE{debug_level} = __LINE__ . <<'END_OF_FUNC';
sub debug_level {
# -------------------------------------------------------
# Set the debug level for either the class or object.
#
    if (ref $_[0]) {
        $_[0]->{_debug} = shift if @_ > 1;
        return $_[0]->{_debug};
    }
    else {
        my $pkg = shift;
        if (@_) {
            my $level = shift;
            ${"${pkg}::DEBUG"} = $level;
        }
        return ${"${pkg}::DEBUG"};
    }
}
END_OF_FUNC

$COMPILE{warn} = __LINE__ . <<'END_OF_FUNC';
sub warn  { shift->error(shift, WARN  => @_) }
END_OF_FUNC

$COMPILE{fatal} = __LINE__ . <<'END_OF_FUNC';
sub fatal { shift->error(shift, FATAL => @_) }
END_OF_FUNC

$COMPILE{error} = __LINE__ . <<'END_OF_FUNC';
sub error {
# -------------------------------------------------------
# Error handler.
#
    my $self    = shift;
    my ($msg, $level, @args) = @_;
    my $pkg     = ref $self || $self;
    $level      = defined $level ? $level : 'FATAL';
    my $is_hash = index($self, 'HASH') != -1;

# Load the ERROR messages.
    $self->set_basic_errors;

# err_pkg stores the package just before the users program for displaying where the error was raised
# think simplified croak.
    my $err_pkg = $pkg;
    if ($is_hash) {
        $err_pkg = defined $self->{_err_pkg} ? $self->{_err_pkg} : $pkg;
    }

# initilize vars to silence -w warnings.
# msg_pkg stores which package error messages are stored, defaults to self, but doesn't have to be.
    ${$pkg . '::ERROR_MESSAGE'} ||= '';
    my $msg_pkg = ${$pkg . "::ERROR_MESSAGE"} ? ${$pkg . "::ERROR_MESSAGE"} : $pkg;
    my $debug = $is_hash ? $self->{_debug} : ${$pkg . "::DEBUG"};

# cls_err stores the actual error hash (error_code => error_string). Initilize to prevent -w
# warnings.
    ${$msg_pkg . '::ERRORS'} ||= {};
    ${$pkg     . '::ERRORS'} ||= {};
    my $cls_err  = ${$msg_pkg . '::ERRORS'};
    my $pkg_err  = ${$pkg     . '::ERRORS'} || $pkg;
    my %messages = %$cls_err;
    foreach (keys %$pkg_err) { $messages{$_} = $pkg_err->{$_}; }

# Return current error if not called with arguments.
    if ($is_hash) {
        $self->{_error} ||= [];
        if (@_ == 0) {
            my @err = @{$self->{_error}} ? @{$self->{_error}} : (${$msg_pkg . "::error"});
            return wantarray ? @err : defined($err[0]) ? $err[0] : undef;
        }
    }
    elsif (@_ == 0) {
        return ${$msg_pkg . '::errcode'};
    }

# Set a subroutine that will clear out the error class vars, and self vars under mod_perl.
    $self->register_persistent_cleanup(sub { $self->_cleanup_obj($msg_pkg, $is_hash) });

# store the error code.
    ${$msg_pkg . '::errcode'} ||= '';
    ${$msg_pkg . '::errcode'} = $msg;
    ${$msg_pkg . '::errargs'} ||= '';
    if ($is_hash) {
        $self->{_errcode} = $msg;
        $self->{_errargs} = @args ? [@args] : [];
    }

# format the error message.
    if (keys %messages) {
        if (exists $messages{$msg}) {
            $msg = $messages{$msg};
        }
        $msg = $msg->(@args) if ref $msg eq 'CODE'; # Pass the sprintf arguments to the code ref
        $msg = @args ? sprintf($msg, map { defined $_ ? $_ : '[undefined]' } @args) : $msg;

        $msg =~ s/\r\n?/\n/g unless $^O eq 'MSWin32';
        $msg =~ s/\n(?=[^ ])/\n\t/g;
    }

# set the formatted error to $msg_pkg::error.
    push @{$self->{_error}}, $msg if ($is_hash);

# If we have a fatal error, then we either send it to error_handler if
# the user has a custom handler, or print our message and die.

# Initialize $error to silence -w warnings.
    ${$msg_pkg . '::error'} ||= '';
    if (uc $level eq 'FATAL') {
        ${$msg_pkg . '::error'} = ref ${$msg_pkg . '::error'} ? _format_err($err_pkg, \$msg) : _format_err($err_pkg, $msg);

        die(_format_err($err_pkg, $msg)) if in_eval();
        if (exists($SIG{__DIE__}) and $SIG{__DIE__}) {
            die _format_err($err_pkg, $msg);
        }
        else {
            print STDERR _format_err($err_pkg, $msg);
            die "\n";
        }
    }
# Otherwise we set the error message, and print it if we are in debug mode.
    elsif (uc $level eq 'WARN') {
        ${$msg_pkg . '::error'} = ref ${$msg_pkg . '::error'} ? \$msg :  $msg;
        my $warning = _format_err($err_pkg, $msg);
        $debug and (
            $SIG{__WARN__}
                ? CORE::warn $warning
                : print STDERR $warning
        );
        $debug and $debug > 1 and (
            $SIG{__WARN__}
                ? CORE::warn stack_trace('GT::Base',1)
                : print STDERR stack_trace('GT::Base',1)
        );
    }
    return;
}
END_OF_FUNC

$COMPILE{_cleanup_obj} = __LINE__ . <<'END_OF_FUNC';
sub _cleanup_obj {
# -------------------------------------------------------
# Cleans up the self object under a persitant env.
#
    my ($self, $msg_pkg, $is_hash) = @_;

    ${$msg_pkg . '::errcode'} = undef;
    ${$msg_pkg . '::error'}   = undef;
    ${$msg_pkg . '::errargs'} = undef;
    if ($is_hash) {
        defined $self and $self->{_errcode} = undef;
        defined $self and $self->{_error}   = undef;
        defined $self and $self->{_errargs} = undef;
    }
    return 1;
}
END_OF_FUNC

$COMPILE{errcode} = __LINE__ . <<'END_OF_FUNC';
sub errcode {
# -------------------------------------------------------
# Returns the last error code generated.
#
    my $self    = shift;
    my $is_hash = index($self, 'HASH') != -1;
    my $pkg     = ref $self || $self;
    my $msg_pkg = ${$pkg . "::ERROR_MESSAGE"} ? ${$pkg . "::ERROR_MESSAGE"} : $pkg;
    if (ref $self and $is_hash) {
        return $self->{_errcode};
    }
    else {
        return ${$msg_pkg . '::errcode'};
    }
}
END_OF_FUNC

$COMPILE{errargs} = __LINE__ . <<'END_OF_FUNC';
sub errargs {
# -------------------------------------------------------
# Returns the arguments from the last error. In list
# context returns an array, in scalar context returns
# an array reference.
#
    my $self    = shift;
    my $is_hash = index($self, 'HASH') != -1;
    my $pkg     = ref $self || $self;
    my $msg_pkg = ${$pkg . "::ERROR_MESSAGE"} ? ${$pkg . "::ERROR_MESSAGE"} : $pkg;
    my $ret = [];
    if (ref $self and $is_hash) {
        $self->{_errargs} ||= [];
        $ret = $self->{_errargs};
    }
    else {
        ${$msg_pkg . '::errcode'} ||= [];
        $ret = ${$msg_pkg . '::errargs'};
    }
    return wantarray ? @{$ret} : $ret;
}
END_OF_FUNC

$COMPILE{clear_errors} = __LINE__ . <<'END_OF_SUB';
sub clear_errors {
# -------------------------------------------------------
# Clears the error stack
#
    my $self = shift;
    $self->{_error}   = [];
    $self->{_errargs} = [];
    $self->{_errcode} = undef;
    return 1;
}
END_OF_SUB

$COMPILE{set_basic_errors} = __LINE__ . <<'END_OF_FUNC';
sub set_basic_errors {
# -------------------------------------------------------
# Sets basic error messages commonly used.
#
    my $self  = shift;
    my $class = ref $self || $self;
    if (${$class . '::ERROR_MESSAGE'}) {
        $class = ${$class . '::ERROR_MESSAGE'};
    }
    ${$class . '::ERRORS'} ||= {};
    my $err = ${$class . '::ERRORS'};
    for my $key (keys %ERRORS) {
        $err->{$key}   = $ERRORS{$key} unless exists $err->{$key};
    }
}
END_OF_FUNC

$COMPILE{whatis} = __LINE__ . <<'END_OF_SUB';
sub whatis {
# -----------------------------------------------------------------------------
# Takes a package name and returns a list of all packages inherited from, in
# the order they would be checked by Perl, _including_ the package passed in.
# The argument may be an object or a string, and this method can be called as
# a function, class method, or instance method. When called as a method, the
# argument is optional - if omitted, the class name will be used.
# Duplicate classes are _not_ included.
#
    shift if @_ > 1;
    my $class = shift;
    $class = ref $class if ref $class;
    my @isa = $class;
    my %found;
    my $pstash;
    for (my $c = 0; $c < @isa; $c++) {
        my $is = $isa[$c];
        my @parts = split /::/, $is;
        my $pstash = $::{shift(@parts) . "::"};
        while (defined $pstash and @parts) {
            $pstash = $pstash->{shift(@parts) . "::"};
        }
        if (defined $pstash and $pstash->{ISA} and my @is = @{*{\$pstash->{ISA}}{ARRAY}}) {
            splice @isa, $c + 1, 0,
                grep $_ eq $class
                    ? die "Recursive inheritance detected in package $class"
                    : !$found{$_}++,
                    @is;
        }
    }
    @isa
}
END_OF_SUB

$COMPILE{in_eval} = __LINE__ . <<'END_OF_FUNC';
sub in_eval {
# -------------------------------------------------------
# Current perl has a variable for it, old perl, we need to look
# through the stack trace. Ugh.
#
    my $ineval;
    if ($] >= 5.005 and !MOD_PERL) { $ineval = defined($^S) ? $^S : (stack_trace('GT::Base',1) =~ /\(eval\)/) }
    elsif (MOD_PERL) {
        my $stack = stack_trace('GT::Base', 1);
        $ineval = $stack =~ m{
            \(eval\)
            (?!
                \s+called\ at\s+
                (?:
                    /dev/null
                |
                    -e
                |
                    /\S*/(?:Apache2?|ModPerl)/(?:Registry(?:Cooker)?|PerlRun)\.pm
                |
                    PerlHandler\ subroutine\ `(?:Apache2?|ModPerl)::Registry
                )
            )
        }x;
    }
    else {
        my $stack = stack_trace('GT::Base', 1);
        $ineval   = $stack =~ /\(eval\)/;
    }
    return $ineval;
}
END_OF_FUNC

$COMPILE{register_persistent_cleanup} = __LINE__ . <<'END_OF_SUB';
sub register_persistent_cleanup {
# -----------------------------------------------------------------------------
# Takes a code reference and registers it for cleanup under mod_perl and
# SpeedyCGI.  Has no effect when not under those environments.
    shift if @_ > 1 and UNIVERSAL::isa($_[0], __PACKAGE__);
    ref(my $code = shift) eq 'CODE'
        or __PACKAGE__->fatal(BADARGS => 'Usage: GT::Base->register_persistent_cleanup($coderef)');

    if (MOD_PERL and MOD_PERL >= 1.999022) { # Final mod_perl 2 API
        require Apache2::ServerUtil;
        if (Apache2::ServerUtil::restart_count() != 1) {
            require Apache2::RequestUtil;
            require APR::Pool;
            Apache2::RequestUtil->request->pool->cleanup_register($code);
        }
    }
    elsif (MOD_PERL and MOD_PERL >= 1.99) { # mod_perl 2 API prior to 2.0.0-RC5
        require Apache2;
        require Apache::ServerUtil;
        if (Apache::ServerUtil::restart_count() != 1) {
            require APR::Pool;
            Apache->request->pool->cleanup_register($code);
        }
    }
    elsif (MOD_PERL and $Apache::Server::Starting != 1) {
        require Apache;
        Apache->request->register_cleanup($code);
    }
    elsif (SPEEDY) {
        CGI::SpeedyCGI->new->register_cleanup($code);
    }

    1;
}
END_OF_SUB

$COMPILE{class_set} = __LINE__ . <<'END_OF_FUNC';
sub class_set {
# -------------------------------------------------------
# Set the class init attributes.
#
    my $pkg     = shift;
    my $attribs = $ATTRIB_CACHE->{$pkg} || _get_attribs($pkg);

    if (ref $attribs ne 'HASH') { return; }

# Figure out what we were passed in.
    my $out = GT::Base->common_param(@_) or return;

# Set the attribs.
    foreach (keys %$out) {
        exists $attribs->{$_} and ($attribs->{$_} = $out->{$_});
    }
}
END_OF_FUNC

$COMPILE{attrib} = __LINE__ . <<'END_OF_FUNC';
sub attrib {
# -------------------------------------------------------
# Returns a list of attributes.
#
    my $class    = ref $_[0] || $_[0];
    my $attribs  = $ATTRIB_CACHE->{$class} || _get_attribs($class);
    return wantarray ? %$attribs : $attribs;
}
END_OF_FUNC

$COMPILE{stack_trace} = __LINE__ . <<'END_OF_FUNC';
sub stack_trace {
# -------------------------------------------------------
# If called with arguments, returns stack trace, otherwise
# prints to stdout/stderr depending on whether in cgi or not.
#
    my $pkg = shift || 'Unknown';
    my $raw = shift || 0;
    my $rollback = shift || 3;
    my ($ls, $spc, $fh);
    my $esc = sub {
        my $t = shift;
        $t =~ s/&/&amp;/g;
        $t =~ s/</&lt;/g;
        $t =~ s/>/&gt;/g;
        $t =~ s/"/&quot;/g;
        $t;
    };
    if ($raw) {
        if (defined $ENV{REQUEST_METHOD}) {
            $ls  = "\n";
            $spc = ' &nbsp; ';
        }
        else {
            $ls  = "\n";
            $spc = ' ';
            $esc = sub { shift };
        }
    }
    elsif (defined $ENV{REQUEST_METHOD}) {
        print STDOUT "Content-type: text/html\n\n";
        $ls = '<br />';
        $spc = '&nbsp;';
        $fh = \*STDOUT;
    }
    else {
        $ls = "\n";
        $spc = ' ';
        $esc = sub { shift };
        $fh = \*STDERR;
    }
    my $out = $raw ? '' : "${ls}STACK TRACE$ls======================================$ls";
    {
        package DB;
        my $i = $rollback;
        local $@;
        while (my ($file, $line, $sub, $args) = (caller($i++))[1,2,3,4]) {
            my @args;
            for (@DB::args) {
                eval { my $a = $_ };     # workaround for a reference that doesn't think it's a reference
                my $print = $@ ? \$_ : $_;
                push @args, defined $print ? $print : '[undef]';
            }
            if (@args) {
                my $args = $esc->(join(", ", @args));
                $args =~ s/\n\s*\n/\n/g;
                $args =~ s/\n/\n$spc$spc$spc$spc/g;
                $out .= qq!$pkg ($$): $sub called at $file line $line with arguments $ls$spc$spc ($args).$ls!;
            }
            else {
                $out .= qq!$pkg ($$): $sub called at $file line $line with no arguments.$ls!;
            }
        }
    }
    $raw ? return $out : print $fh $out;
}
END_OF_FUNC

$COMPILE{_format_err} = __LINE__ . <<'END_OF_FUNC';
sub _format_err {
# -------------------------------------------------------
# Formats an error message for output.
#
    my ($pkg, $msg) = @_;
    my ($file, $line) = get_file_line($pkg);
    return "$pkg ($$): $msg at $file line $line.\n";
}
END_OF_FUNC

$COMPILE{get_file_line} = __LINE__ . <<'END_OF_FUNC';
sub get_file_line {
# -------------------------------------------------------
# Find out what line error was generated in.
#
    shift if @_ > 1 and UNIVERSAL::isa($_[0], __PACKAGE__);
    my $pkg = shift || scalar caller;
    my %pkg;
    for (whatis($pkg)) {
        $pkg{$_}++;
    }
    my ($i, $last_pkg);
    while (my $pack = caller($i++)) {
        if ($pkg{$pack}) {
            $last_pkg = $i;
        }
        elsif ($last_pkg) {
            last; # We're one call back beyond the package being looked for
        }
    }
    unless (defined $last_pkg) {
        # You messed up by trying to pass in a package that was never called
        GT::Base->fatal("get_file_line() called with an invalid package ($pkg)");
    }
    (undef, my ($file, $line)) = caller($last_pkg);

    return ($file, $line);
}
END_OF_FUNC

$COMPILE{_generate_fatal} = __LINE__ . <<'END_OF_FUNC';
sub _generate_fatal {
# -------------------------------------------------------------------
# Generates a fatal error caused by misuse of AUTOLOAD.
#
    my ($self, $attrib, $param) = @_;
    my $is_hash = index($self, 'HASH') != -1;
    my $pkg     = ref $self || $self;

    my @poss;
    if (UNIVERSAL::can($self, 'debug_level') and $self->debug_level) {
        my @class = @{$pkg . '::ISA'} || ();
        unshift @class, $pkg;
        for (@class) {
            my @subs = keys %{$_ . '::'};
            my %compiled = %{$_ . '::COMPILE'};
            for (keys %compiled) {
                push @subs, $_ if defined $compiled{$_};
            }
            for my $routine (@subs) {
                next if $attrib eq $routine;
                next unless $self;
                next unless defined $compiled{$_} or UNIVERSAL::can($self, $routine);
                if (GT::Base->_sndex($attrib) eq GT::Base->_sndex($routine)) {
                    push @poss, $routine;
                }
            }
        }
    }

# Generate an error message, with possible alternatives and die.
    my $err_pkg = $is_hash ? (defined $self->{_err_pkg} ? $self->{_err_pkg} : $pkg) : $pkg;
    my ($call_pkg, $file, $line) = caller(1);
    my $msg = @poss
        ? "    Perhaps you meant to call " . join(", or " => @poss) . ".\n"
        : '';
    die "$err_pkg ($$): Unknown method '$attrib' called at $file line $line.\n$msg";
}
END_OF_FUNC

$COMPILE{_sndex} = __LINE__ . <<'END_OF_FUNC';
sub _sndex {
# -------------------------------------------------------
# Do a soundex lookup to suggest alternate methods the person
# might have wanted.
#
    my $self = shift;
    local $_ = shift;
    my $search_sound = uc;
    $search_sound =~ tr/A-Z//cd;
    if ($search_sound eq '') { $search_sound = 0 }
    else {
        my $f = substr($search_sound, 0, 1);
        $search_sound =~ tr/AEHIOUWYBFPVCGJKQSXZDTLMNR/00000000111122222222334556/;
        my $fc = substr($search_sound, 0, 1);
        $search_sound =~ s/^$fc+//;
        $search_sound =~ tr///cs;
        $search_sound =~ tr/0//d;
        $search_sound = $f . $search_sound . '000';
        $search_sound = substr($search_sound, 0, 4);
    }
    return $search_sound;
}
END_OF_FUNC

1;


} # End of BEGIN for GT/Base.pm

BEGIN {
    $INC{"GT/Dumper.pm"} = "GT/Dumper.pm";

# ==================================================================
# Gossamer Threads Module Library - http://gossamer-threads.com/
#
#   GT::Dumper
#   Author: Scott Beck 
#   $Id: Dumper.pm,v 1.39 2007/02/10 15:59:02 sbeck Exp $
# 
# Copyright (c) 2004 Gossamer Threads Inc.  All Rights Reserved.
# ==================================================================
#
# Description:
#   Implements a data dumper, useful for converting complex Perl
#   data structures to strings, which can then be eval()ed back to
#   the original value.
#

package GT::Dumper;
# ===============================================================
use strict;
use vars qw /$DEBUG $ATTRIBS $VERSION @EXPORT @ISA $EOL/;
use GT::Base;
use Exporter;
use overload;

$EOL     = "\n";
$VERSION = sprintf "%d.%03d", q$Revision: 1.39 $ =~ /(\d+)\.(\d+)/;
$ATTRIBS = {
    var       => '$VAR',
    data      => undef,
    sort      => 1,
    order     => undef,
    compress  => undef,
    structure => undef,
    tab       => '    '
};
@EXPORT = qw/Dumper/;
@ISA    = qw/Exporter GT::Base/;

sub Dumper {
# -----------------------------------------------------------
#   Dumper acts similar to Dumper in Data::Dumper when called as a
#   class method. If called as a instance method it assumes you
#   have set the options for the dump and does not change them.
#   It only takes a single argument - the variable to dump.
#
    my $self;
    if (@_ == 2 and UNIVERSAL::isa($_[0], __PACKAGE__)) {
        $self = shift;
        $self->{data} = shift;
    }
    elsif (@_ == 1) {
        $self = GT::Dumper->new(data => shift);
    }
    else {
        die "Bad args to Dumper()";
    }
    return $self->dump;
}

sub dump {
# -----------------------------------------------------------
# my $dump = $class->dump(%opts);
# --------------------------------
#   Returns the data structure specified in %opts flatened.
#   %opts is optional if you have created an object with the
#   options.
#
    my $this  = shift;

# See if options were passed in
    my $self;
    if (!ref $this) {
        $self = $this->new(@_);
    }
    else {
        $self = $this;
        if (@_) {
            my $data = $self->common_param(@_) or return $self->fatal(BADARGS => '$dumper->dump(%opts)');
            $self->set($data);
        }
    }

    my $level = 0;
    my $ret = '';
    if ($self->{var} and not $self->{structure}) {
        $ret .= ($self->{compress} ? "$self->{var}=" : "$self->{var} = ");
    }
    $self->_dump_value($level + 1, $self->{data}, \$ret);
    $ret .= ';' unless $self->{structure};
    $ret .= $EOL unless $self->{structure} or $self->{compress};

    return $ret ? $ret : 1;
}

sub dump_structure {
    my ($self, $data) = @_;
    return $self->dump(structure => 1, data => $data);
}

sub _dump_value {
# -----------------------------------------------------------
# Internal method to decide what to dump.
#
    my ($self, $level, $val, $ret, $n) = @_;
    my $was;
    my $ref = ref $val;
    if    ($ref and overload::StrVal($val) =~ /=/) { $self->_dump_obj(  $level + 1, $val, $ret) }
    elsif ($ref eq 'HASH') {       $self->_dump_hash( $level + 1, $val, $ret) }
    elsif ($ref eq 'ARRAY') {      $self->_dump_array($level + 1, $val, $ret) }
    elsif ($ref eq 'SCALAR' or $ref eq 'REF' or $ref eq 'LVALUE') {
        $self->_dump_scalar($level, $val, $ret)
    }
    elsif ($ref eq 'CODE') { $$ret .= 'sub { () }' }
    else { $$ret .= _escape($val) }
    return 1;
}

sub _dump_scalar {
# -----------------------------------------------------------
# Dump a scalar reference.
#
    my ($self, $level, $val, $ret, $n) = @_;
    my $v = $$val;
    $$ret .= '\\';
    $self->_dump_value($level, $v, $ret, 1);
    return 1;
}

sub _dump_hash {
# -----------------------------------------------------------
# Internal method to for through a hash and dump it.
#
    my ($self, $level, $hash_ref, $ret) = @_;
    $$ret .= '{';
    my $lines;
    if ($self->{sort}) {
        for (sort { ref($self->{order}) eq 'CODE' ? $self->{order}->($a, $b, $hash_ref->{$a}, $hash_ref->{$b}) : $a cmp $b } keys %{$hash_ref}) {
            $$ret .= "," if $lines++;
            $$ret .= $EOL . ($self->{tab} x ($level / 2)) unless $self->{compress};
            my $key = _escape($_);
            $$ret .= $self->{compress} ? "$key," : "$key => ";
            $self->_dump_value($level + 1, $hash_ref->{$_}, $ret, 1);
        }
    }
    else {
        for (keys %{$hash_ref}) {
            $$ret .= "," if $lines++;
            $$ret .= $EOL . ($self->{tab} x ($level / 2)) unless $self->{compress};
            my $key = _escape($_);
            $$ret .= $self->{compress} ? "$key," : "$key => ";
            $self->_dump_value($level + 1, $hash_ref->{$_}, $ret, 1);
        }
    }
    $$ret .= $EOL if $lines and not $self->{compress};
    $$ret .= ($lines and not $self->{compress}) ? (($self->{tab} x (($level - 1) / 2)) . "}") : "}";
    return 1;
}

sub _dump_array {
# -----------------------------------------------------------
# Internal method to for through an array and dump it.
#
    my ($self, $level, $array_ref, $ret) = @_;
    $$ret .= "[";
    my $lines;
    for (@{$array_ref}) {
        $$ret .= "," if $lines++;
        $$ret .= $EOL.($self->{tab} x ($level / 2)) unless $self->{compress};
        $self->_dump_value($level + 1, $_, $ret, 1);
    }
    $$ret .= ($lines and not $self->{compress}) ? $EOL.(($self->{tab} x (($level - 1) / 2)) . "]") : "]";
    return 1;
}

sub _dump_obj {
# -----------------------------------------------------------
# Internal method to dump an object.
#
    my ($self, $level, $obj, $ret) = @_;
    my $class = ref $obj;
    $$ret .= "bless(";
    $$ret .= $EOL.($self->{tab} x ($level / 2)) unless $self->{compress};
    my $strval = overload::StrVal($obj);
    if ($strval =~ /ARRAY\(/)                      { $self->_dump_array($level + 2, \@{$obj}, $ret) }
    elsif ($strval =~ /HASH\(/)                    { $self->_dump_hash( $level + 2, \%{$obj}, $ret) }
    elsif ($strval =~ /SCALAR\(/ or $obj =~ /REF\(/ or $obj =~ /LVALUE\(/)
                                                { $self->_dump_value($level + 2, $$obj, $ret)    }
    $$ret .= ",";
    $$ret .= $EOL.($self->{tab} x ($level / 2)) unless $self->{compress};
    $$ret .= _escape($class);
    $$ret .= $EOL.($self->{tab} x (($level - 1) / 2)) unless $self->{compress};
    $$ret .= ")";
    return 1;
}


sub _escape {
# -----------------------------------------------------------
# Internal method to escape a dumped value.
    my ($val) = @_;
    defined($val) or return 'undef';
    $val =~ s/('|\\(?=['\\]|$))/\\$1/g;
    return "'$val'";
}

1;


} # End of BEGIN for GT/Dumper.pm

BEGIN {
    $INC{"bases.pm"} = "bases.pm";

# ==================================================================
# Gossamer Threads Module Library - http://gossamer-threads.com/
#
#   bases
#   Author: Scott Beck
#   $Id: bases.pm,v 1.9 2004/01/13 01:35:15 jagerman Exp $
#
# Copyright (c) 2004 Gossamer Threads Inc.  All Rights Reserved.
# ==================================================================

package bases;

use strict 'subs', 'vars';

sub import {
    my $class = shift;
    my $pkg = caller;
    my $hsh = {@_};
    my @indices = map { $_[$_ * 2] } 0 .. $#_ * 0.5;
    foreach my $base (@indices) {
        next if $pkg->isa($base);
        push @{"$pkg\::ISA"}, $base;
        my $args = '';
        if (my $ref = ref $hsh->{$base}) {
            require GT::Dumper;
            if ($ref eq 'ARRAY') {
                $args = '(@{' . GT::Dumper->dump_structure($hsh->{$base}) . '})';
            }
            else {
                $args = '(' . GT::Dumper->dump_structure($hsh->{$base}) . ')';
            }
        }
        elsif (defined $hsh->{$base}) {
            $args = $hsh->{$base} eq '' ? '()' : "qw($hsh->{$base})";
        }
        my $dcl = qq|
            package $pkg;
            use $base $args;
        |;
        eval $dcl;
        die "$@: $dcl" if $@ && $@ !~ /^Can't locate .*? at \(eval /;
        unless (defined %{"$base\::"}) {
            require Carp;
            Carp::croak(
qq|Base class package "$base" is empty.
String:
$dcl
\t(Perhaps you need to 'use' the module which defines that package first.)|
            );
        }
    }
}

1;


} # End of BEGIN for bases.pm

BEGIN {
    $INC{"GT/TempFile.pm"} = "GT/TempFile.pm";

# ==================================================================
# Gossamer Threads Module Library - http://gossamer-threads.com/
#
#   GT::TempFile
#   Author  : Scott Beck
#   $Id: TempFile.pm,v 1.36 2005/03/23 04:27:26 jagerman Exp $
#
# Copyright (c) 2004 Gossamer Threads Inc.  All Rights Reserved.
# ==================================================================
#
# Description:
#   Implements a tempfile.
#

package GT::TempFile;
# ===================================================================

# Pragmas
use strict;
use vars   qw/$VERSION $TMP_DIR %OBJECTS/;
use bases 'GT::Base' => ':all';
use overload '""' => \&as_string;

$VERSION = sprintf "%d.%03d", q$Revision: 1.36 $ =~ /(\d+)\.(\d+)/;

sub find_tmpdir {
# -------------------------------------------------------------------
# Sets the tmpdir.
#
    return $TMP_DIR if $TMP_DIR;
    my @tmp_dirs;
    for (qw/GT_TMPDIR TEMP TMP TMPDIR/) {
        push @tmp_dirs, $ENV{$_} if exists $ENV{$_};
    }
    push @tmp_dirs, $ENV{windir} . '/temp' if exists $ENV{windir};
    eval { push @tmp_dirs, (getpwuid $>)[7] . '/tmp' };
    push @tmp_dirs, '/usr/tmp', '/var/tmp', 'c:/temp', '/tmp', '/temp', '/sys$scratch', '/WWW_ROOT', 'c:/windows/temp', 'c:/winnt/temp';

    for my $dir (@tmp_dirs) {
        return $TMP_DIR = $dir if $dir and -d $dir and -w _ and -x _;
    }
    $TMP_DIR = '.';
}

sub new {
# -----------------------------------------------------------------------------
# Create a new tempfile.
#
    $TMP_DIR ||= find_tmpdir();
    my $self = bless {}, 'GT::TempFile::Tmp';
    $self->reset;

# Backwards compatibility
    if ( @_ == 2 and not ref( $_[1] ) ) {
        ( $self->{tmp_dir} ) = $_[1];
    }
    elsif ( @_ > 1 ) {
        $self->set( @_[1 .. $#_] );
    }

    my $dir      = $self->{tmp_dir} || $TMP_DIR;
    my $count    = substr(time, -4) . int(rand(10000));
    my $filename = '';

# Directory for locking
    my $lock_dir = "$dir/$self->{prefix}GT_TempFile_lock";

# W need to create the directory
    my $safety = 0;
    until ( mkdir( $lock_dir, 0777 ) ) {

# If we wait 10 seconds and still no lock we assume the lockfile is stale
        if ( $safety++ > 10 ) {
            rmdir $lock_dir or $self->fatal( 'RMDIR', $lock_dir, "$!" );
        }
        sleep 1;
    }

# Now lets get our temp file
    for (1 .. 20) {
        $filename = "$dir/$self->{prefix}GTTemp$count";
        last if (! -f $filename);
        $count++;
    }

# If the open fails we need to remove the lockdir
    if ( !open( FH, ">$filename" ) ) {
        rmdir $lock_dir or $self->fatal( 'RMDIR', $lock_dir, "$!" );
        $self->fatal( 'WRITEOPEN', $filename, "$!" );
    }
    close FH;

# All done searching for a temp file, now release the directory lock
    rmdir $lock_dir or $self->fatal( 'RMDIR', $lock_dir, "$!" );
    ($filename =~ /^(.+)$/) and ($filename = $1); # Detaint.

    $self->{filename} = $filename;
    my $object = bless \$filename, 'GT::TempFile';
    $OBJECTS{overload::StrVal $object} = $self;
    $self->debug("New tmpfile created ($filename).") if ($self->{_debug});
    $object;
}

sub as_string {
# -------------------------------------------------------------------
# Backwards compatibility
    my ( $self ) = @_;
    return $$self;
}

sub DESTROY {
# -------------------------------------------------------------------
    my $obj = shift;
    my $self = $OBJECTS{$obj};
    $self->debug("Deleteing $self->{filename}") if $self->{_debug};

# unlink the file if they wanted it deleted
    if ($self->{destroy}) {
        unless (unlink $self->{filename}) {
            $self->debug("Unable to remove temp file: $self->{filename} ($!)") if $self->{_debug};
        }
    }
    delete $OBJECTS{$obj};
}

package GT::TempFile::Tmp;
use bases 'GT::Base' => '';
use vars qw/$ATTRIBS $ERRORS/;
$ATTRIBS = {
    prefix  => '',
    destroy => 1,
    tmp_dir => undef,
};
$ERRORS = { SAFETY => "Safety reached while trying to create lock directory %s, (%s)" };

1;


} # End of BEGIN for GT/TempFile.pm

BEGIN {
    $INC{"GT/Tar.pm"} = "GT/Tar.pm";

# ==================================================================
# Gossamer Threads Module Library - http://gossamer-threads.com/
#
#   GT::Tar
#   Author: Scott Beck
#   $Id: Tar.pm,v 1.57 2006/08/28 23:17:11 brewt Exp $
#
# Copyright (c) 2004 Gossamer Threads Inc.  All Rights Reserved.
# ==================================================================
#
# Description: A general purpose taring and untaring module.
#

package GT::Tar;
# ==================================================================
# Pragmas
use vars qw/$DEBUG $ERRORS $FAKE_GETPWUID $HAVE_GZIP $FAKE_GETGRGID $FH/;
use strict;

# System modules
use Fcntl;
use Symbol qw/gensym/;

use constants
    BLOCK => 4096,

    # 'Type' constants - these are the actual chars, not the char values
    FILE     => 0,
    HARDLINK => 1,
    SYMLINK  => 2,
    CHARDEV  => 3,
    BLOCKDEV => 4,
    DIR      => 5,
    FIFO     => 6,
    SOCKET   => 8,
    UNKNOWN  => 9,
    LONGNAME => 'L',

    FORMAT_HEADER_PACK => 'a100 a8 a8 a8 a12 a12 A8 a1 a100 a6 a2 a32 a32 a8 a8 a155 x12',
    FORMAT_HEADER_UNPACK => 'A100 A8 A8 A8 A12 A12 A8 A1 A100 A6 A2 A32 A32 A8 A8 A155';

# Internal modules
use GT::Base;

# Globals
$DEBUG = 0;
@GT::Tar::ISA = qw{GT::Base};

$ERRORS = {
    OPEN     => "Could not open '%s': %s",
    READ     => "There was an error reading from '%s'.  Expected to read %s bytes, but only got %s",
    BINMODE  => "Could not binmode '%s': %s",
    BADARGS  => "Bad arguments passed to %s: %s",
    CHECKSUM => "Checksum error parsing tar file.  Most likely this is a corrupt tar.\nHeader: %s\nChecksum: %s\nFile: %s\n",
    NOBODY   => "File '%s' has no body!",
    CANTFIND => "Unable to find a file named '%s' in tar archive",
    CHMOD    => "Could not chmod '%s': %s",
    DIRFILE  => "Unable to create directory: '%s' exists and is a file",
    MKDIR    => "Could not mkdir '%s': %s",
    RENAME   => "Unable to rename temporary file '%s' to '%s': %s",
    NOGZIP   => "Compress::Zlib module is required to work with .tar.gz files"
};

$FAKE_GETPWUID = "unknown" if ($^O eq 'MSWin32');
$FAKE_GETGRGID = "unknown" if ($^O eq 'MSWin32');
$HAVE_GZIP     = eval { local $SIG{__DIE__}; require Compress::Zlib; 1; } ? 1 : 0;
$FH            = 0;

sub new {
# ------------------------------------------------------------------------------
# GT::Tar->new('/path/to/new/tar.tar');
# --------------------------------------
#   Constructor for GT::Tar. Call this method to create a new archive.
#   To do anything with an existing archive call GT::Tar->open.
#
    my $this  = shift;
    my $class = ref $this || $this;
    my $self  = bless {}, $class;

    my $opt = {};
    if (@_ == 1) { $opt->{io} = shift }
    else {
        $opt = $self->common_param(@_);
    }

    $self->{_debug} = exists $opt->{debug} ? $opt->{debug} : $DEBUG;
    $opt->{io} or return $self->fatal(BADARGS => "new()", "No output archive passed in");

    $opt->{io} =~ /^(.+)$/;
    $self->{clean} = exists $opt->{clean} ? $opt->{clean} : 1;
    my $file = $1;

# If it's a gz file, store the name in gz_file, and work off a temp file.
    if ($file =~ /\.t?gz$/) {
        $HAVE_GZIP or return $self->warn('NOGZIP');
        require GT::TempFile;
        my $tmpfile = new GT::TempFile;
        $self->{file}     = $$tmpfile;     # Filename of ungzipped tar file.
        $self->{gz_file}  = $file;         # Filename of gzipped file.
        $self->{tmp_file} = $tmpfile;      # Don't unlink it till the object is destroyed.
    }
    else {
        $self->{file} = $file;
    }
    $self->{io} = gensym;
    sysopen $self->{io}, $self->{file}, O_CREAT|O_TRUNC|O_RDWR or return $self->fatal(OPEN => $self->{file}, "$!");
    binmode $self->{io} or return $self->fatal(BINMODE => $self->{file}, "$!");
    select((select($self->{io}), $| = 1)[0]);

    $self->{parsed}    = 0;
    $self->{new_tar}   = 1;
    return $self;
}

sub open {
# ------------------------------------------------------------------------------
# GT::Tar->open('/path/to/tar.tar');
# -----------------------------------
#   Opens the tar specified by the first argument for reading and calls
#   $obj->parse to parse the contents.
#   Returns a new GT::Tar object.
#
    my $this  = shift;
    my $class = ref $this || $this;
    my $self  = bless {}, $class;

    my $opt = {};
    if (@_ == 1) { $opt->{io} = shift }
    else {
        $opt = $self->common_param(@_);
    }

    $self->{_debug} = exists $opt->{debug} ? $opt->{debug} : $DEBUG;
    $opt->{io} or return $self->fatal(BADARGS => "open()", "No input archive passed in");

    $opt->{io} =~ /^(.+)$/;
    my $file = $1;

# If it's a gz file, uncompress it to a temp file and work off that.
    if ($file =~ /\.t?gz$/) {
        $HAVE_GZIP or return $self->warn('NOGZIP');
        require GT::TempFile;
        my $tmpfile = new GT::TempFile;
        $self->debug("Decompressing gz file to temp file: $$tmpfile") if ($self->{_debug});
        open(FH, "> $$tmpfile") or return $self->warn(OPEN => $$tmpfile, "$!");
        binmode FH;
        my $gz = Compress::Zlib::gzopen($file, 'rb') or return $self->warn(OPEN => $file, $Compress::Zlib::gzerrno);
        my $line;
        while ($gz->gzreadline($line)) {
            print FH $line;
        }
        close FH;

        $gz->gzclose;
        $self->{file}      = $$tmpfile;     # Filename of open ungzipped tar file.
        $self->{gz_file}   = $file;         # Filename of original gzipped file.
        $self->{tmp_file}  = $tmpfile;      # Don't unlink it till the object is destroyed.
    }
    else {
        $self->{file} = $file;
    }
    $self->{io} = gensym;
    $self->debug("Opening $file") if ($self->{_debug});
    sysopen $self->{io}, $self->{file}, O_RDONLY or return $self->warn(OPEN => $self->{file}, "$!");
    binmode $self->{io} or return $self->warn(BINMODE => $self->{file} => "$!");
    select((select($self->{io}), $| = 1)[0]);

    my $parts = $self->parse;
    defined $parts or return;
    $self->{new_tar} = 0;
    return $self;
}

sub close_tar {
# ------------------------------------------------------------------------------
# Closes the tar file.
#
    my $self = shift;
    $self->{parsed} = 0;
    close $self->{io} if ($self->{io} and defined fileno($self->{io}));
}
sub DESTROY { my $self = shift; $self->close_tar; }

sub parse {
# ------------------------------------------------------------------------------
# Modified from code in Archive::Tar
# Untar a file, specified by first argument to directories, specified in third
# argument, and set the path to perl, specified in second argument, to all .pl
# and .cgi files
#
    my $self = shift;
    $self->{parts} = [];
    my ($head, $msg);
    my $tar = $self->{io}
        or return $self->fatal(BADARGS => "parse", "An IO must be defined to parse");

    seek($tar, 0, 0);
    my $longname;
    READLOOP: while (read($tar, $head, 512) and length($head) == 512) {
# End of archive
        last READLOOP if $head eq "\0" x 512;

# Apparently this should really be two blocks of 512 zeroes, but GNU tar
# sometimes gets it wrong. See comment in the source code (tar.c) to GNU cpio.

        my $file = GT::Tar::Parts->format_read($head);

        $self->debug("Looking at $file->{name}") if ($self->{_debug});

        substr($head, 148, 8) = "        ";
        if ($file->{type} eq FILE) {
            if (unpack("%16C*", $head) != $file->{chksum}) {
                return $self->warn(CHECKSUM => $head, $file->{chksum}, $file->{name});
            }
        }

        if ($file->{type} eq FILE || $file->{type} eq LONGNAME) {
# Find the start and the end positions in the tar file for the body of the tar
# part
            my $start = tell $tar;
            seek($tar,  $file->{size}, 1);
            $file->body([$tar, $start]);

# Seek off trailing garbage.
            my $block = $file->{size} & 0x01ff ? ($file->{size} & ~0x01ff) + 512 : $file->{size};
            my $to_read = $block - $file->{size};
            if ($to_read) { seek($tar, $to_read, 1) }
        }

        if ($longname and $file->{type} ne LONGNAME) {
            my $filename = $longname->body_as_string;
            $filename =~ s|\0.*||s;
            $file->name($filename);
        }

        $file->name(_clean($file->name)) if $self->{clean};

# Guard against tarfiles with garbage at the end
        last READLOOP if $file->{name} eq '';

        if ($file->{type} eq LONGNAME) {
            $longname = $file;
        }
        else {
            $longname = undef;
# Sanity check - make sure a file doesn't end in a /
            if ($file->{type} == GT::Tar::FILE and $file->{name} =~ m|/$|) {
                $file->{type} = GT::Tar::DIR;
            }
            push(@{$self->{parts}}, $file);
        }
    }
    $self->{parsed} = 1;
    seek($tar, 0, 0);
    return wantarray ? @{$self->{parts}} : $self->{parts};
}

sub _clean {
# -----------------------------------------------------------------------------
# Sanitises a path, removing anything up to a .. path component, and removing
# any leading /'s.
#
    my $path = shift;
    $path =~ s#.*(?:^|/)\.\.(/|$)#$1#;
    $path =~ s#^/+##;
    $path;
}

sub untar {
# -----------------------------------------------------------------------------
# $obj->untar(\&code);
# ---------------------
#   Untars tar file specified in $obj->open and runs callback for each entry in
#   the tar file. Passed a parts object to that callback.
#
# $obj->untar;
# ------------
#   Same a above but no callback.
#
# GT::Tar->untar('/path/to/tar.tar', \&code);
# -------------------------------------------
#   Untars file specified by the first argument and runs callback in second
#   argument.
#
# GT::Tar->untar('/path/to/tar.tar', '/path/for/extraction');
# -----------------------------------------------------------
#   Untars tar file specified in first argument into path specified in second
#   argument.
#
    my $self = ref $_[0] eq __PACKAGE__ ? shift : shift->open(shift);

    my $callback = pop;
    if (ref $callback) {
        (ref $callback eq 'CODE')
            or return $self->fatal(BADARGS => "untar", "Callback that was passed in was not a code ref");
    }
    elsif ($callback) {
        -d $callback or return $self->fatal(BADARGS => untar => "Extraction path '$callback' does not exist");
    }

    if (!$self->{parsed}) {
        $self->debug("Parsing tar file") if ($self->{_debug});
        $self->parse or return;
    }
    else {
        $self->debug("Already parsed") if ($self->{_debug});
    }

    for (@{$self->{parts}}) {
        if (ref $callback eq 'CODE') {
            $callback->($_);
        }
        else {
            $_->write($callback || ());
        }
    }
    return $self;
}

sub tar {
# ------------------------------------------------------------------------------
# $obj->tar;
# ----------
#   Creates tar file that was specified in $obj->new with files that were added
#   using $obj->add.
#
# GT::Tar->tar('/path/to/tar.tar', @files);
# ------------------------------------------
#   Creates tar file specified by the first argument with the files specified
#   by the remaining arguments.
#
    my $self;
    if (ref $_[0] eq __PACKAGE__) {
        $self = shift;
    }
    else {
        my $class = shift;
        $self  = $class->new( io => shift );
        $self->add(@_) if (@_);
    }
    $self->write;
}

sub write {
# ------------------------------------------------------------------------------
# $obj->write;
# ------------
#   Creates all the files that are internally in the parts objects.  You add
#   files to parts by calling $obj->add -or- by calling $obj->open on an
#   existing tar file. This is similar to untar.
#
    my $self = shift;
    my ($out, $rename, $filename);

# Working off an existing tar file.
    if (! $self->{new_tar}) {
        if (@_) {
            $filename = shift;

# If we have a new .tar.gz file, we need to write it to a tmp .tar first.
            if ($filename =~ /\.t?gz$/) {
                $HAVE_GZIP or return $self->warn('NOGZIP');
                $self->{gz_file} = $filename;
                undef $filename;
            }
        }
        if (! $filename) {
            require GT::TempFile;
            my $tmp = new GT::TempFile;
            $filename = $$tmp;
            $rename   = $self->{file};
        }
        $out = gensym;
        sysopen $out, $filename, O_CREAT|O_TRUNC|O_RDWR or return $self->warn(OPEN => $filename, "$!");
        binmode $out or return $self->fatal(BINMODE => $filename, "$!");
    }
# Working off a new tar file.
    else {
        $out = $self->{io};
        seek($out, 0, 0);
    }

# Unbuffer output
    select((select($out), $| = 1)[0]);
    foreach my $entry (@{$self->{parts}}) {
        my $head = $entry->format_write;
        print $out $head;
        my $save = tell $out;
        if ($entry->type == FILE) {
            my $bh;
            my $body = $entry->body or return $self->warn(NOBODY => $entry->name);
            my $ref  = ref $body;
            if ($ref eq 'GLOB' and defined fileno $body) {
                my $fh = $body;
                my $pos  = tell $fh;
                binmode $fh;
                while (read $fh, $_, BLOCK) {
                    print $out $_;
                }
                seek($fh, $pos, 0);
            }
            elsif ($ref eq 'ARRAY') {
                my ($reads, $rem, $data, $pos);
                my ($fh, $start) = @{$body};
                $pos = tell $fh;
                seek($fh, $start, 0);
                binmode $fh;
                $reads = int($entry->{size} / BLOCK);
                $rem   = $entry->{size} % BLOCK;
                for (1 .. $reads) {
                    my $read = read($fh, $data, BLOCK);
                    ($read == BLOCK)
                        or return $self->warn(READ => join(',' => @{$body}), BLOCK, $read);
                    print $out $data;
                }
                if ($rem) {
                    my $read = read($fh, $data, $rem);
                    ($read == $rem)
                        or return $self->warn(READ => join(',' => @{$body}), $rem, $read);
                    print $out $data;
                }
                seek($fh, $pos, 0);
            }
            elsif ($ref eq 'SCALAR') {
                CORE::open F, ${$body} or return $self->warn(READOPEN => ${$body}, "$!");
                binmode F;
                while (read F, $_, BLOCK) {
                    print $out $_;
                }
                close F;
            }
            else {
                print $out $body;
            }
            my $size = $entry->{size} & 511;
            if ($size) {
                print $out ("\0" x (512 - $size));
            }
            $entry->body( [ $out, $save ] );
        }
    }
    print $out ("\0" x 1024);

# Copy the temp file over to the original file (can't rename across filesystems).
    if ($rename and !$self->{gz_file}) {
        seek($out, 0, 0);
        $self->{io} = gensym;
        sysopen($self->{io}, $rename, O_CREAT|O_TRUNC|O_RDWR) or return $self->warn(OPEN => $rename, "$!");
        binmode $self->{io};
        while (read($out, my $buffer, BLOCK)) {
            print {$self->{io}} $buffer;
        }
        seek($self->{io}, 0, 0);

# Need to set the parts to the new file handle.
        foreach my $entry (@{$self->{parts}}) {
            if ($entry->type == FILE) {
                $entry->{body}->[0] = $self->{io};
            }
        }
        close $out;
        $out = $self->{io};
        $self->{file} = $rename;
        unlink $filename or return $self->warn(UNLINK => $filename, "$!");
    }

# Recompress if it was a .gz file.
    if ($self->{gz_file}) {
        $HAVE_GZIP or return $self->warn('NOGZIP');
        seek($out, 0, 0);
        my $gz = Compress::Zlib::gzopen($self->{gz_file}, 'wb') or return $self->warn(OPEN => $self->{gz_file}, $Compress::Zlib::gzerrno);
        while (read($out, my $buffer, BLOCK)) {
            $gz->gzwrite($buffer);
        }
        $gz->gzclose();
        seek($out, 0, 0);
    }
    return 1;
}

sub extract {
# ------------------------------------------------------------------------------
# $obj->extract(@list);
# ----------------------
# $obj->extract(\@list);
# -----------------------
#   Extracts only the files specified in @list from the working tar file. No
#   files are extracted if none are in memory.
#
    my $self  = shift;
    my %files = map { $_ => 1 } ref($_[0]) eq 'ARRAY' ? @{$_[0]} : @_;
    my $num = '0E0';
    foreach my $entry (@{$self->{parts}}) {
        next unless (exists $files{$entry->{name}});
        $entry->write;
        $num++;
    }
    return $num;
}

sub add_file {
# ------------------------------------------------------------------------------
# $obj->add_file(@list);
# ------------------
# $obj->add_file(\@list);
# -------------------
#   Adds the files specified in @list to the in-memory archive.
#
    my $self  = shift;
    my @files = ref $_[0] eq 'ARRAY' ? @{$_[0]} : @_;

    while (my $file = shift @files or @files) {
        next if not defined $file;
        my ($mode, $nlnk, $uid, $gid, $rdev, $size, $mtime, $type, $linkname);

        $self->debug("Looking at $file") if ($self->{_debug});
        if (($mode, $nlnk, $uid, $gid, $rdev, $size, $mtime) = (lstat $file)[2 .. 7, 9]) {
            $linkname = "";
            $type = filetype($file);

            $linkname = readlink $file if ($type == SYMLINK);
            if ($type == DIR) {
                my $dir = gensym;
                opendir $dir, $file or return $self->warn(OPEN => "Can't add directory '$file'", "$!");
                push(@files, map { $file . '/' . $_ } grep !/^\.\.?$/, readdir $dir);
                closedir $dir;
            }

            my $part = GT::Tar::Parts->new(
                {
                    name     => $file,
                    mode     => $mode,
                    uid      => $uid,
                    gid      => $gid,
                    size     => $size,
                    mtime    => ($mtime | 0),
                    chksum   => "      ",
                    magic    => "ustar",
                    version  => "",
                    type     => $type,
                    linkname => $linkname,
                    devmajor => 0, # We don't handle this yet
                    devminor => 0, # We don't handle this yet
                    uname    => ($FAKE_GETPWUID || scalar getpwuid($uid)),
                    gname    => ($FAKE_GETGRGID || scalar getgrgid($gid)),
                    prefix   => "",
                }
            );
            if ($type == FILE) {
                $self->debug("Adding $file to as body") if ($self->{_debug});
                $part->body(\$file);
            }
            push(@{$self->{parts}}, $part);

        }
        else {
            $self->debug("Could not stat file '$file'");
        }
    }
    return wantarray ? @{$self->{parts}} : $self->{parts};
}

sub remove_file {
# -------------------------------------------------------------------
# Takes a string and removes the file from the tar.
#
    my ($self, $filename) = @_;
    return unless (defined $filename);
    @{$self->{parts}} = grep { $_->{name} ne $filename } @{$self->{parts}};
}

sub get_file {
# -------------------------------------------------------------------
# Returns the file object of a given file name.
#
    my ($self, $filename) = @_;
    return unless (defined $filename);
    my @files = grep { $_->{name} eq $filename } @{$self->{parts}};
    if (! @files) {
        return $self->warn(CANTFIND => $filename);
    }
    return wantarray ? @files : shift @files;
}

sub add_data {
# -------------------------------------------------------------------
# $obj->add_newfile( { ... } );
# ------------------------------
#   Adds a file from a hash ref of part attributes.
#
    my $self = shift;
    my $part = @_ > 1 ? {@_} : shift;
    ref $part eq 'HASH' or return $self->fatal(BADARGS => "Usage: \$obj->add_newfile( part options )");

    defined $part->{name} or return $self->fatal(BADARGS => "You must supply a file name.");
    defined $part->{body} or return $self->fatal(BADARGS => "You must supply a body for the file.");

    if (ref $part->{body}) {
        if (defined fileno $part->{body}) {
            local $/;
            my $fh = $part->{body};
            $part->{body} = <$fh>;
        }
        else {
            return $self->fatal(BADARGS => "You must supply either a scalar or a file handle to body");
        }
    }
    my $file = GT::Tar::Parts->new({
        name     => $part->{name},
        mode     => defined $part->{mode}  ? $part->{mode} : 0666 & (0777 - umask),
        uid      => defined $part->{uid}   ? $part->{uid}  : $>,
        gid      => defined $part->{gid}   ? $part->{gid}  : (split(/ /,$)))[0],
        size     => length $part->{body},
        mtime    => defined $part->{mtime} ? $part->{mtime} : time,
        chksum   => "      ",
        magic    => "ustar",
        version  => "00",
        type     => FILE,
        linkname => '',
        devmajor => 0, # We don't handle this yet
        devminor => 0, # We don't handle this yet
        uname    => ($FAKE_GETPWUID || scalar getpwuid(defined $part->{uid} ? int($part->{uid}) : $>)),
        gname    => ($FAKE_GETGRGID || scalar getgrgid(defined $part->{gid} ? int($part->{gid}) : (split(/ /,$)))[0])),
        prefix   => ""
    });
    $file->body($part->{body});
    push(@{$self->{parts}}, $file);
    return $file;
}

sub files {
# ------------------------------------------------------------------------------
# my @files = $obj->files;
# ------------------------
#   Returns a list of the part objects that are in the in-memory archive.
#   Returns an array ref in scalar context.
#
    my @parts = defined $_[0]->{parts} ? @{$_[0]->{parts}} : ();
    return wantarray ? @parts : \@parts;
}

sub filetype {
# ------------------------------------------------------------------------------
# Internal method. filetype -- Determine the type value for a given file
#
    my $file = shift;

    return SYMLINK  if (-l $file);  # Symlink
    return FILE     if (-f _);      # Plain file
    return DIR      if (-d _);      # Directory
    return FIFO     if (-p _);      # Named pipe
    return SOCKET   if (-S _);      # Socket
    return BLOCKDEV if (-b _);      # Block special
    return CHARDEV  if (-c _);      # Character special
    return UNKNOWN; # Something else (like what?)
}

package GT::Tar::Parts;
# ==================================================================
# Pragmas
use vars qw/$DEBUG $ERRORS $ATTRIBS $ERROR_MESSAGE/;
use strict;

# System modules
use Fcntl;
use Symbol qw/gensym/;

# Globals
$DEBUG = $GT::Tar::DEBUG;
@GT::Tar::Parts::ISA = qw{GT::Base};
$ATTRIBS = {
     name      => '',
     mode      => '',
     uid       => '',
     gid       => '',
     size      => '',
     mtime     => '',
     chksum    => "      ",
     type      => '',
     linkname  => '',
     magic     => "ustar",
     version   => undef,
     uname     => 'unknown',
     gname     => 'unknown',
     devmajor  => 0, # We don't handle this yet
     devminor  => 0, # We don't handle this yet
     prefix    => "",
     body      => undef,
     set_owner => 1,
     set_perms => 1,
     set_time  => 1,
};
$ERROR_MESSAGE = 'GT::Tar';

sub format_read {
# ------------------------------------------------------------------------------
# my $obj = GT::Tar::Parts->format_read($heading);
# -------------------------------------------------
#   Unpacks the string that is passed in. The string need to be a valid header
#   from a single entry in a tar file. Return a new object for the Tar part.
#   You will need to set the body yourself after calling this.
#
    my $head_tainted = pop;
    my ($head) = $head_tainted =~ /(.+)/;
    my $file = {};
    (
        $file->{name},     $file->{mode},
        $file->{uid},      $file->{gid},
        $file->{size},     $file->{mtime},
        $file->{chksum},   $file->{type},
        $file->{linkname}, $file->{magic},
        $file->{version},  $file->{uname},
        $file->{gname},    $file->{devmajor},
        $file->{devminor}, $file->{prefix}
    ) = unpack(GT::Tar::FORMAT_HEADER_UNPACK, $head);

    $file->{uid}      = oct $file->{uid};
    $file->{gid}      = oct $file->{gid};
    $file->{mode}     = oct $file->{mode};
    $file->{size}     = oct $file->{size};
    $file->{mtime}    = oct $file->{mtime};
    $file->{chksum}   = oct $file->{chksum};
    $file->{devmajor} = oct $file->{devmajor};
    $file->{devminor} = oct $file->{devminor};
    $file->{name}     = $file->{prefix} . "/" . $file->{name} if $file->{prefix};
    $file->{prefix}   = "";

    return GT::Tar::Parts->new($file);
}

sub format_write {
# ------------------------------------------------------------------------------
# $obj->format_write;
# -------------------
#   Formats the current object's header for writing to a tar file.  Returns the
#   formatted string.  In the case of a file with a path+name longer than 100
#   characters (in other words, longer than can fit in the tar's filename
#   field), this actually returns a longlink header + longlink body + file
#   header.
#
    my $self = shift;

    my $file = $self->{name};
    if ($self->{type} == GT::Tar::DIR and $file !~ m,/$,) {
        $file .= '/';
    }

    my $longlink;
    if (length($file) > 100) {
        my $body = $file . "\0"; # GNU tar produces a long link file with a body ending with a \0; copy it.
        $longlink = pack(
            GT::Tar::FORMAT_HEADER_PACK,
            '././@LongLink', # Filename
            sprintf('%07o', 0), # mode
            sprintf('%07o', 0), # uid
            sprintf('%07o', 0), # gid
            sprintf('%011o', length $body), # size
            sprintf('%011o', 0), # mtime
            '', # checksum
            GT::Tar::LONGNAME, # type
            'ustar', ' ',
            'root', # username (Using 'root' copied from GNU tar)
            'root', # group name (Using 'root' copied from GNU tar)
            '', # devmajor
            '', # devminor
            '' # prefix
        );
        substr($longlink, 148, 7) = sprintf("%06o\0", unpack("%16C*", $longlink));
        $longlink .= $body;

        my $pad_from = length($body) % 512;
        if ($pad_from) {
            $longlink .= "\0" x (512 - $pad_from);
        }
    }

    my $header = pack(
        GT::Tar::FORMAT_HEADER_PACK,
        $file,
        sprintf("%07o", $self->{mode}),
        sprintf("%07o", $self->{uid}),
        sprintf("%07o", $self->{gid}),
        sprintf("%011o", $self->{type} == GT::Tar::DIR ? 0 : $self->{size}),
        sprintf("%011o", $self->{mtime}),
        "",        #checksum field - space padded by pack("A8")
        $self->{type},
        $self->{linkname},
        ($self->{magic} eq 'ustar' and !$self->{version})
            ? ('ustar ', ' ') # oldgnu format, which treated magic+version as a contiguous field with a value of "ustar  \0"
            : ($self->{magic}, $self->{version} || '00'),
        $self->{uname},
        $self->{gname},
        '', # sprintf("%6o ",$self->{devmajor}),
        '', # sprintf("%6o ",$self->{devminor}),
        '' # prefix
    );
    substr($header, 148, 7) = sprintf("%06o\0", unpack("%16C*", $header));
    $header = $longlink . $header if $longlink;
    return $header;
}

sub body {
# ------------------------------------------------------------------------------
# my $path = $obj->body;
# ----------------------
# $obj->body(\'/path/to/body');
# $obj->body("My body text.");
# -----------------------------
#   Sets or gets the path to the body of this tar part. If a scalar ref is
#   passed in it is considered a path to a file otherwize it is considered a
#   string to write to the body when write is called.
#
    my ($self, $io) = @_;
    !$io and return $self->{body};
    $self->{body} = $io;
    my $ref = ref $io;
    if ($ref eq 'GLOB' and defined fileno $io) {
        $self->{size} = (lstat(${$self->{body}}))[7];
    }
    elsif ($ref eq 'SCALAR') {
        $self->{size} = -s ${$self->{body}};
    }
    elsif (not $ref) {
        $self->{size} = length $self->{body};
    }

    return $self->{body};
}

sub body_as_string {
# ------------------------------------------------------------------------------
# my $data = $obj->body_as_string;
# --------------------------------
#   Returns the body of the file as a string.
#
    my $self = shift;
    my $data = '';
    my $ref  = ref $self->{body};
    if ($ref eq 'GLOB' and defined fileno $self->{body}) {
        my $fh = $self->{body};
        my $pos = tell $fh;
        seek($fh, 0, 0);
        binmode $fh;
        local $/;
        $data = <$fh>;
        seek($fh, $pos, 0);
    }
    elsif ($ref eq 'ARRAY') {
        my ($fh, $start) = @{$self->{body}};
        my $pos = tell $fh;
        binmode $fh;
        seek($fh, $start, 0);
        read($fh, $data, $self->{size});
        seek($fh, $pos, 0);
    }
    elsif ($ref eq 'SCALAR') {
        my $fh = gensym;
        open $fh, ${$self->{body}} or return $self->warn(READOPEN => ${$self->{body}}, "$!");
        binmode $fh;
        read($fh, $data, -s $fh);
        close $fh;
    }
    else {
        $data = $self->{body};
    }
    return $data;
}

sub write {
# ------------------------------------------------------------------------------
# $obj->write;
# ------------
#   Writes this part to disk using the path that is in $obj->body. This function
#   will recursively make the directories needed to create the structure of this
#   part.
#
#   An optional extraction path can be passed in - if provided, extraction will
#   be based in that directory instead of the current directory.
#
    my ($self, $extract_to) = @_;

# For the moment, we assume that all paths in tarfiles are given according to
# Unix standards, which they *are*, according to the tar format spec!
    $self->_write_dir($extract_to) or return;
    if ($self->{type} == GT::Tar::FILE) {
        my $out = gensym;
        my $name = ($self->{name} =~ /^(.+)$/s)[0];
        $name = "$extract_to/$name" if $extract_to;
        open $out, "> $name" or return $self->warn(OPEN => $name, "$!");
        binmode $out or return $self->warn(BINMODE => $name => "$!");
        my $ref  = ref $self->{body};
        if ($ref eq 'GLOB' and defined fileno $self->{body}) {
            my $fh = $self->{body};
            my $pos = tell $fh;
            binmode $fh;
            while (read $fh, $_, GT::Tar::BLOCK) {
                print $out $_;
            }
            seek($fh, $pos, 0);
        }
        elsif ($ref eq 'ARRAY') {
            my ($reads, $rem, $data, $pos);
            my ($fh, $start) = @{$self->{body}};
            $pos = tell $fh;
            seek($fh, $start, 0);
            binmode $fh;
            $reads = int($self->{size} / GT::Tar::BLOCK);
            $rem   = $self->{size} % GT::Tar::BLOCK;
            for (1 .. $reads) {
                my $read = read($fh, $data, GT::Tar::BLOCK);
                ($read == GT::Tar::BLOCK)
                    or return $self->warn(READ => join(',' => @{$self->{body}}), GT::Tar::BLOCK, $read);
                print $out $data;
            }
            if ($rem) {
                my $read = read($fh, $data, $rem);
                ($read == $rem)
                    or return $self->warn(READ => join(',' => @{$self->{body}}), $rem, $read);
                print $out $data;
            }
            seek($fh, $pos, 0);
        }
        elsif ($ref eq 'SCALAR') {
            my $fh = gensym;
            open $fh, ${$self->{body}} or return $self->warn(READOPEN => ${$self->{body}}, "$!");
            binmode $fh;
            while (read $fh, $_, GT::Tar::BLOCK) {
                print $out $_;
            }
            close $fh;
        }
        else {
            print $out $self->{body};
        }
        close $out;
        $self->debug("Created $self->{name} size $self->{size}") if ($self->{_debug});
    }
    $self->_file_sets;

    return 1;
}

sub _write_dir {
# ------------------------------------------------------------------------------
# Internal method used to create a directory for a file, or just create a
# directory if this is a directory part and the directory does not exist.
#
    my ($self, $base_dir) = @_;

    my $name = $self->{name};
    $name = "$base_dir/$name" if defined $base_dir;

    if ($self->{type} == GT::Tar::DIR) {
        -e $name and not -d _ and return $self->fatal(DIRFILE => $name);
        -d _ or $self->_recurse_mkdir($base_dir) or return;
    }
    else {
        $self->_recurse_mkdir($base_dir) or return;
    }
    return 1;
}

sub _recurse_mkdir {
# -----------------------------------------------------------------------------
# Internal method to recursivly make a directory.  If the directory contains ..
# path components, everything up to the last one is removed.  Likewise, a path
# starting with / has the initial / removed.
#
    my ($self, $base_dir) = @_;
    my $dir  = $self->{name};
    $dir = "$base_dir/$dir" if defined $base_dir;
    my @path = split m|/|, $dir;
    pop @path unless substr($dir, -1) eq '/';

    my @subpath; # /foo/bar/baz/ -> ('/foo/bar/baz', '/foo/bar', '/foo', '')
    for (reverse 0 .. $#path) {
        push @subpath, join '/', @path[0 .. $_], '';
    }
    push @subpath, '.' unless substr($dir, 0, 1) eq '/' or $subpath[-1] eq '.' or $subpath[-1] eq './';
    for my $i (0 .. $#subpath) {
        my $path = $subpath[$i];
        next if $path eq '';
        if (-e $path and not -d _) { return $self->warn(DIRFILE => $path) }
        elsif (-d _) {
            for (reverse 0 .. $i-1) {
                next if -d $subpath[$_];
                mkdir $subpath[$_], 0777 or return $self->warn(MKDIR => $subpath[$_], "$!");
                $self->debug("mkdir $subpath[$_]") if $DEBUG;
            }
            last;
        }
    }
    return 1;
}

sub _file_sets {
# ------------------------------------------------------------------------------
# Internal method to set the file or directory permissions and or onership of
# this part.
#
    my $self = shift;

# Set the file creation time.
    if ($self->{set_time}) {
        utime time, $self->{mtime}, $self->{name};
    }

# Set the file owner.
    if ($self->{set_owner}) {
        $self->debug("chown ($self->{uid},$self->{gid}) $self->{name}") if ($self->{_debug});
        chown($self->{uid}, $self->{gid}, $self->{name})
            if ($> == 0 and $^O ne "MacOS" and $^O ne "MSWin32");
    }

# Set the permissions (done last in case it makes file readonly)
    if ($self->{set_perms}) {
        my ($mode) = sprintf("%lo", $self->{mode}) =~ /(\d{3})$/;
        $self->debug("chmod $mode, $self->{name}") if ($self->{_debug});
        chmod $self->{mode}, $self->{name} or return $self->warn(CHMOD => $self->{name}, "$!");
    }

    return 1;
}

1;


} # End of BEGIN for GT/Tar.pm

BEGIN {
    $INC{"GT/CGI.pm"} = "GT/CGI.pm";

# ==================================================================
# Gossamer Threads Module Library - http://gossamer-threads.com/
#
#   GT::CGI
#   Author  : Aki Mimoto
#   $Id: CGI.pm,v 1.158 2008/06/09 23:39:24 brewt Exp $
# 
# Copyright (c) 2004 Gossamer Threads Inc.  All Rights Reserved.
# ==================================================================
#
# Description:
#   Implements CGI.pm's CGI functionality, but faster.
#

package GT::CGI;
# ===============================================================
use strict;
use GT::Base(':persist'); # Imports MOD_PERL, SPEEDY and PERSIST
use vars qw/@ISA $DEBUG $VERSION $ATTRIBS $ERRORS $PRINTED_HEAD $EOL $TAINTED
            $FORM_PARSED %PARAMS @PARAMS %COOKIES @EXPORT_OK %EXPORT_TAGS/;
use GT::AutoLoader;
require Exporter;

@ISA     = qw/GT::Base/;
$DEBUG   = 0;
$VERSION = sprintf "%d.%03d", q$Revision: 1.158 $ =~ /(\d+)\.(\d+)/;
$ATTRIBS = {
    nph  => 0,
    p    => '',
    upload_hook => undef
};
$ERRORS  = {
    INVALIDCOOKIE => "Invalid cookie passed to header: %s",
    INVALIDDATE   => "Date '%s' is not a valid date format.",
};

# Used to append to strings that need tainting because they were passed through
# a regex, but should be tainted.
{
    local $^W = 0;
    $TAINTED = substr("$0$^X", 0, 0);
}

$EOL          = ($^O eq 'MSWin32') ? "\n" : "\015\012"; # IIS has problems with \015\012 on nph scripts.
$PRINTED_HEAD = 0;
$FORM_PARSED  = 0;
%PARAMS       = ();
@PARAMS       = ();
%COOKIES      = ();
@EXPORT_OK    = qw/escape unescape html_escape html_unescape/;
%EXPORT_TAGS  = (
    escape => [qw/escape unescape html_escape html_unescape/]
);

# Pre load our compiled if under mod_perl/speedy.
if (PERSIST) {
    require GT::CGI::Cookie;
    require GT::CGI::MultiPart;
    require GT::CGI::Fh;
}

sub load_data {
#--------------------------------------------------------------------------------
# Loads the form information into PARAMS. Data comes from either a multipart
# form, a GET Request, a POST request, or as arguments from command line.
#
    my $self = shift;
    unless ($FORM_PARSED) {

# If we are under mod_perl we let mod_perl know that it should call reset_env
# when a request is finished.
        GT::Base->register_persistent_cleanup(\&reset_env);

# Reset all the cache variables
        %PARAMS = @PARAMS = %COOKIES = ();

# Load form data.
        my $method         = defined $ENV{REQUEST_METHOD}   ? uc $ENV{REQUEST_METHOD} : '';
        my $content_length = defined $ENV{'CONTENT_LENGTH'} ? $ENV{'CONTENT_LENGTH'} : 0;

        if ($method eq 'GET' or $method eq 'HEAD') {
            $self->parse_str(defined $ENV{QUERY_STRING} ? $ENV{QUERY_STRING} : '');
        }
        elsif ($method eq 'POST') {
            if ($content_length) {
                if ($ENV{CONTENT_TYPE} and $ENV{CONTENT_TYPE} =~ /^multipart/) {
                    require GT::CGI::MultiPart;
                    GT::CGI::MultiPart->parse($self, $self->upload_hook);
                }
                else {
                    read(STDIN, my $data, $content_length, 0);
                    if ($ENV{CONTENT_TYPE} and $ENV{CONTENT_TYPE} !~ m|^application/x-www-form-urlencoded|) {
                        $self->{post_data} = $data . $TAINTED;
                    }
                    else {
                        $data =~ s/\r?\n/&/g;
                        $self->parse_str($data);
                    }
                }
            }
        }
        else {
            my $data = join "&", @ARGV;
            $self->parse_str($data);
        }

# Load cookies.
        if (defined $ENV{HTTP_COOKIE}) {
            for (split /;\s*/, $ENV{HTTP_COOKIE}) {
                /(.*)=(.*)/ or next;
                my ($key, $val) = (unescape($1 . $TAINTED), unescape($2 . $TAINTED));
                $val = [split '&', $val];
                $self->{cookies}->{$key} = $val;
            }
        }
        else {
            %{$self->{cookies}} = ();
        }

# Parse form buttons, allowing you to pass in name="foo=bar;a=b;c=d" as a name
# tag in the form.
        for (keys %{$self->{params}}) {
            if (index($_, '=') >= 0) {
                next if substr($_, -2) eq '.y';
                (my $key = $_) =~ s/\.x$//;
                $self->parse_str($key);
            }
        }

# Save the data for caching
        while (my ($k, $v) = each %{$self->{params}}) {
            push @{$PARAMS{$k}}, @$v;
        }
        while (my ($k, $v) = each %{$self->{cookies}}) {
            push @{$COOKIES{$k}}, @$v;
        }
        @PARAMS = @{$self->{param_order} || []};

# Make sure the form is not parsed again during this request
        $FORM_PARSED = 1;
    }
    else { # Load the data from the cache
        while (my ($k, $v) = each %PARAMS) {
            push @{$self->{params}->{$k}}, @$v;
        }
        while (my ($k, $v) = each %COOKIES) {
            push @{$self->{cookies}->{$k}}, @$v;
        }
        $self->{param_order} = [@PARAMS];
    }


    $self->{data_loaded} = 1;
}

sub class_new {
# --------------------------------------------------------------------------------
# Creates an object to be used for all class methods, this affects the global
# cookies and params.
#
    my $self = bless {} => shift;
    $self->load_data unless $self->{data_loaded};

    $self->{cookies} = \%COOKIES;
    $self->{params}  = \%PARAMS;
    $self->{param_order} = \@PARAMS;

    for (keys %{$ATTRIBS}) { $self->{$_} = $ATTRIBS->{$_} }

    return $self;
}

sub reset_env {
# --------------------------------------------------------------------------------
# Reset the global environment.
#
    %PARAMS = @PARAMS = %COOKIES = ();
    $PRINTED_HEAD = $FORM_PARSED = 0;
    1;
}

sub init {
#--------------------------------------------------------------------------------
# Called from GT::Base when a new object is created.
#
    my $self = shift;

# If we are passed a single argument, then we load our data from
# the input.
    if (@_ == 1) {
        my $p = $_[0];
        if (ref $p eq 'GT::CGI') {
            $p = $p->query_string;
        }
        $self->parse_str($p ? "&$p" : "");
        if (defined $ENV{HTTP_COOKIE}) {
            for (split /;\s*/, $ENV{HTTP_COOKIE}) {
                /(.*)=(.*)/ or next;
                my ($key, $val) = (unescape($1 . $TAINTED), unescape($2 . $TAINTED));
                $val = [split '&', $val];
                $self->{cookies}->{$key} = $val;
            }
        }
        $self->{data_loaded} = 1;
        $FORM_PARSED = 1;
    }
    elsif (@_) {
        $self->set(@_);
    }
    return $self;
}

$COMPILE{get_hash} = __LINE__ . <<'END_OF_SUB';
sub get_hash {
#-------------------------------------------------------------------------------
# Returns the parameters as a HASH, with multiple values becoming an array
# reference.
#
    my $self = shift;
    $self = $self->class_new unless ref $self;
    $self->load_data() unless $self->{data_loaded};
    my $join = defined $_[0] ? $_[0] : 0;

    keys %{$self->{params}} or return {};

# Construct hash ref and return it
    my $opts = {};
    foreach (keys %{$self->{params}}) { 
        my @vals = @{$self->{params}->{$_}};
        $opts->{$_} = @vals > 1 ? \@vals : $vals[0];
    }
    return $opts;
}
END_OF_SUB

$COMPILE{delete} = __LINE__ . <<'END_OF_SUB';
sub delete {
#--------------------------------------------------------------------------------
# Remove an element from the parameters.
#
    my ($self, $param) = @_;
    $self = $self->class_new unless ref $self;
    $self->load_data() unless $self->{data_loaded};
    my @ret;
    if (exists $self->{params}->{$param}) {
        @ret = @{delete $self->{params}->{$param}};
        for (my $i = 0; $i < @{$self->{param_order}}; $i++) {
            if ($self->{param_order}->[$i] eq $param) {
                splice @{$self->{param_order}}, $i, 1;
                last;
            }
        }
    }
    return wantarray ? @ret : $ret[0];
}
END_OF_SUB

$COMPILE{cookie} = __LINE__ . <<'END_OF_SUB';
sub cookie {
#--------------------------------------------------------------------------------
# Creates a new cookie for the user, implemented just like CGI.pm.
#
    my $self = shift;
    $self = $self->class_new unless ref $self;
    $self->load_data() unless $self->{data_loaded};
    if (@_ == 0) {    # Return keys.
        return keys %{$self->{cookies}};
    }
    elsif (@_ == 1) { # Return value of param passed in.
        my $param = shift;
        return unless defined $param and $self->{cookies}->{$param};
        return wantarray ? @{$self->{cookies}->{$param}} : $self->{cookies}->{$param}->[0];
    }
    elsif (@_ == 2) {
        require GT::CGI::Cookie;
        return GT::CGI::Cookie->new(-name => $_[0], -value => $_[1]);
    }
    elsif (@_ % 2 == 0) {
        my %data = @_;
        if (exists $data{'-value'}) {
            require GT::CGI::Cookie;
            return GT::CGI::Cookie->new(%data);
        }
    }
    $self->fatal("Invalid arguments to cookie()");
}
END_OF_SUB

sub param {
#--------------------------------------------------------------------------------
# Mimick CGI's param function for get/set.
#
    my $self = shift;
    $self = $self->class_new unless ref $self;
    $self->load_data() unless $self->{data_loaded};
    if (@_ == 0) {    # Return keys in the same order they were provided
        return @{$self->{param_order} || []};
    }
    elsif (@_ == 1) { # Return value of param passed in.
        my $param = shift;
        return unless (defined($param) and $self->{params}->{$param});
        return wantarray ? @{$self->{params}->{$param}} : $self->{params}->{$param}->[0];
    }
    else { # Set parameter.
        my ($param, $value) = @_;
        unless ($self->{params}->{$param}) {
            # If we're not replacing/changing a parameter, we need to add the param to param_order
            push @{$self->{param_order}}, $param;
        }
        $self->{params}->{$param} = [ref $value eq 'ARRAY' ? @$value : $value];
    }
}

sub header {
#--------------------------------------------------------------------------------
# Mimick the header function.
#
    my $self = shift;
    $self = $self->class_new unless ref $self;
    my %p = ref $_[0] eq 'HASH' ? %{$_[0]} : @_ % 2 ? () : @_;
    my @headers;

# Don't print headers twice unless -force'd.
    return '' if not delete $p{-force} and $PRINTED_HEAD;

# Start by adding NPH headers if requested.
    my $status = $p{-permanent} ? 301 : 302;
    if ($self->{nph} || $p{-nph}) {
        if ($p{-url}) {
            push @headers, "HTTP/1.0 $status Moved";
        }
        else {
            my $protocol = $ENV{SERVER_PROTOCOL} || 'HTTP/1.0';
            unless (MOD_PERL) {
                push @headers, "$protocol 200 OK";
            }
        }
    }
    elsif ($p{-url}) {
        push @headers, "Status: $status Moved";
    }
    delete @p{qw/nph -nph/};

# If requested, add a "Pragma: no-cache"
    my $no_cache = $p{'no-cache'} || $p{'-no-cache'};
    delete @p{qw/no-cache -no-cache/};
    if ($no_cache) {
        require GT::Date;
        push @headers,
            "Expires: Tue, 25 Jan 2000 12:00:00 GMT",
            "Last-Modified: " . GT::Date::date_get_gm(time, "%ddd%, %dd% %mmm% %yyyy% %HH%:%MM%:%ss% GMT"),
            "Cache-Control: no-store",
            "Pragma: no-cache";
    }

# Add any cookies, we accept either an array of cookies
# or a single cookie.
    my $add_date = 0;
    my $cookies  = 0;
    my $container = delete($p{-cookie}) || '';
    require GT::CGI::Cookie if $container;
    if (ref $container and UNIVERSAL::isa($container, 'GT::CGI::Cookie')) {
        my $c = $container->cookie_header;
        push @headers, $c;
        $add_date = 1;
        $cookies++;
    }
    elsif (ref $container eq 'ARRAY') {
        foreach my $cookie (@$container) {
            next unless (defined $cookie and (ref $cookie eq 'GT::CGI::Cookie'));
            push @headers, $cookie->cookie_header;
            $add_date = 1;
            $cookies++;
        }
    }
    elsif ($container) {
        $self->error('INVALIDCOOKIE', 'WARN', $container);
    }

# Print expiry if requested.
    if (defined(my $expires = delete $p{-expires})) {
        require GT::CGI::Cookie;
        my $date = GT::CGI::Cookie->format_date(' ', $expires);
        unless ($date) {
            $self->error('INVALIDDATE', 'WARN', $expires);
        }
        else {
            push @headers, "Expires: $date";
            $add_date = 1;
        }
    }

# Add a Date header if we printed an expires tag or a cookie tag.
    if ($add_date) {
        require GT::CGI::Cookie;
        my $now = GT::CGI::Cookie->format_date(' ');
        push @headers, "Date: $now";
    }

# Add Redirect Header.
    my $iis_redirect;
    if (my $url = delete $p{-url}) {
        $url =~ s/[\x00-\x08\x0a-\x1f].*//s;
# IIS 3-5 will drop any cookie headers on a redirect
# http://support.microsoft.com/kb/q176113
        if ($ENV{SERVER_SOFTWARE} =~ m|IIS/[3-5]|i and $cookies) {
            $iis_redirect = $url;
# Remove the Status: 301/302 header
            for (my $i = 0; $i < @headers; $i++) {
                if ($headers[$i] =~ /^Status:\s*30[12]/i) {
                    splice @headers, $i, 1;
                    last;
                }
            }
        }
        else {
            push @headers, "Location: $url";
        }
    }

# Add the Content-type header.
    my $type = @_ == 1 && !ref($_[0]) ? $_[0] : delete($p{-type}) || 'text/html';
    my $charset = delete $p{-charset};
    if ($charset and $type =~ /^text\// and $type !~ /\bcharset\b/) {
        $type .= "; charset=$charset";
    }
    push @headers, "Content-type: $type";

# Add any custom headers.
    foreach my $key (keys %p) {
        $key =~ /^\s*-?(.+)/;
        push @headers, escape(ucfirst $1) . ": " . (ref $p{$key} eq 'SCALAR' ? ${$p{$key}} : escape($p{$key}));
    }
    $PRINTED_HEAD = 1;

    my $headers = '';
    for (@headers) {
        # Control characters other than tab aren't allowed; remove any - but not \n, which we handle later:
        y/\x00-\x08\x0b-\x1f//d;

        # Newlines are allowed if followed by a space or tab (this is header
        # folding - RFC 2616 § 2.2).  If we encounter any *not* followed by a
        # space, force one in - the alternative would be to delete it entirely,
        # but that arguably isn't much better or worse.
        s/\x0a+$//;
        s/\x0a(?![ \t])/\x0a /g;
        s/\x0a/$EOL/g unless $EOL eq "\x0a";
        $headers .= $_ . $EOL;
    }
    $headers .= $EOL;

# Fun hack for IIS
    if ($iis_redirect) {
        $iis_redirect =~ y/;/&/; # You can't have semicolons in a meta http-equiv tag.
        return $headers . <<END_OF_HTML;
<!DOCTYPE HTML PUBLIC "-//W3C//DTD HTML 4.01 Transitional//EN" "http://www.w3.org/TR/html4/transitional.dtd">
<html><head><title>Document Moved</title><meta http-equiv="refresh" content="0;URL=$iis_redirect"><meta http-equiv="content-type" content="text/html; charset=us-ascii"></head>
<body><noscript><h1>Object Moved</h1>This document may be found <a HREF="$iis_redirect">here</a></noscript></body></html>
END_OF_HTML
    }
    return $headers;
}

$COMPILE{redirect} = __LINE__ . <<'END_OF_SUB';
sub redirect {
#-------------------------------------------------------------------------------
# Print a redirect header.
#
    my $self = shift;
    $self = $self->class_new unless ref $self;

    my (@headers, $url);
    if (@_ == 0) {
        return $self->header({ -url => $self->self_url });
    }
    elsif (@_ == 1) {
        return $self->header({ -url => shift });
    }
    else {
        my $opts = ref $_[0] eq 'HASH' ? shift : {@_};
        $opts->{'-url'} ||= $opts->{'-URL'} || $self->self_url;
        return $self->header($opts);
    }
}
END_OF_SUB

$COMPILE{file_headers} = __LINE__ . <<'END_OF_SUB';
sub file_headers {
# -----------------------------------------------------------------------------
# Returns a list of header arguments that can be passed into header() when
# sending a file.  Takes a hash (not hash ref or GT::CGI object or ...) of
# options:
#   filename - filename being sent; required
#   mimetype - mime-type to send; defaults to using GT::MIMETypes with filename
#   inline - set to true to send an inline content disposition, false to send
#            attachment.  Not specifying this key causes inline to be sent for
#            common image and text types, and attachment to be sent for
#            everything else.  You generally should either not specify this, or
#            set it to false (i.e. to force a download prompt).
#   size - size of the data to be sent; optional, but recommended as
#          Content-Length and Content-Disposition size won't be set without it.
#
    my $self = __PACKAGE__;
    $self = shift if @_ > 1 and UNIVERSAL::isa($_[0], __PACKAGE__);
    my %args = @_;
    my $filename = $args{filename};
    defined $filename and length $filename or $self->fatal(BADARGS => "No filename passed to file_headers");

    my $mimetype;
    unless ($mimetype = $args{mimetype}) {
        $mimetype = eval { require GT::MIMETypes; GT::MIMETypes::guess_type($filename) } || 'application/octet-stream';
    }
    my $cd;
    if (exists $args{inline}) {
        $cd = $args{inline} ? 'inline' : 'attachment';
    }
    else {
        $cd = $mimetype =~ m{^(?:image/(?:gif|jpeg|png|bmp)|application/pdf|text/(?:plain|html))$}
            ? 'inline'
            : 'attachment';
    }
    my $size = $args{size};
    $size = int $size if defined $size;

    # First, sanitize the filename so that people can't create extra HTTP headers by embedding
    # a \n in the filename.  So, strip out all control characters (except tab):
    $filename =~ y/\x00-\x08\x0a-\x1f//d;

    # As for filename escaping, every browser seems to want something different to get the
    # filename correct.  Mozilla and Opera appear to be the only browsers that gets things
    # right - enclose the filename in "", and \escape every \\ and ".  IE doesn't work with
    # that - it needs the filename to be URL escaped.  Konqueror and Safari cannot handle
    # arbitrary filenames at all - it handles neither of the above escape methods, so you could
    # easily break out of the quoted structure and send Content-Disposition values directly to
    # the browser.  So, Safari/Konqueror get "'s stripped out, as well as \'s (in case they
    # ever fix their browser).
    # Filename:         asdf asdf"\foo"\zxc vxzcv
    # Konqueror/Safari: filename="asdf adsf"\foo"\zxc vxzcv" # broken, we set: filename="asdf asdffoozxc vxzcv"
    # IE:               filename="asdf%20asdf%22%5Cfoo%22%5Czxc%20vxzcv"
    # Mozilla/Opera/standard-compliant: filename="asdf asdf\"\\foo\"\\zxc vxzcv"
    my %browser = browser_info();
    if ($browser{is_ie}) {
        $filename = escape($filename);
    }
    elsif ($browser{is_konqueror} or $browser{is_safari}) {
        $filename =~ y/\\"//d;
    }
    else {
        $filename =~ s/([\\"])/\\$1/g;
    }

    return (
        -type => $mimetype,
        "Content-Disposition" => \(qq/$cd; filename="$filename"/ . (defined($size) ? "; size=$size" : '')),
        defined($size) ? ("Content-Length" => $size) : ()
    );
}
END_OF_SUB

sub unescape {
#-------------------------------------------------------------------------------
# returns the url decoded string of the passed argument. Optionally takes an
# array reference of multiple strings to decode. The values of the array are
# modified directly, so you shouldn't need the return (which is the same array
# reference).
#
    my $todecode = pop;
    return unless defined $todecode;
    for my $str (ref $todecode eq 'ARRAY' ? @$todecode : $todecode) {
        $str =~ tr/+/ /; # pluses become spaces
        $str =~ s/%([0-9a-fA-F]{2})/chr(hex($1))/ge;
    }
    $todecode;
}

$COMPILE{escape} = __LINE__ . <<'END_OF_SUB';
sub escape {
#--------------------------------------------------------------------------------
# return the url encoded string of the passed argument
#
    my $toencode = pop;
    return unless defined $toencode;
    $toencode =~ s/([^\w\-.!~*'()])/sprintf("%%%02X",ord($1))/eg;
    return $toencode;
}
END_OF_SUB

$COMPILE{html_escape} = __LINE__ . <<'END_OF_SUB';
sub html_escape {
#--------------------------------------------------------------------------------
# Return the string html_escaped.
#
    my $toencode = pop;
    return unless defined $toencode;
    if (ref($toencode) eq 'SCALAR') {
        $$toencode =~ s/&/&amp;/g;
        $$toencode =~ s/</&lt;/g;
        $$toencode =~ s/>/&gt;/g;
        $$toencode =~ s/"/&quot;/g;
        $$toencode =~ s/'/&#039;/g;
    }
    else {
        $toencode =~ s/&/&amp;/g;
        $toencode =~ s/</&lt;/g;
        $toencode =~ s/>/&gt;/g;
        $toencode =~ s/"/&quot;/g;
        $toencode =~ s/'/&#039;/g;
    }
    return $toencode;
}
END_OF_SUB

$COMPILE{html_unescape} = __LINE__ . <<'END_OF_SUB';
sub html_unescape {
#--------------------------------------------------------------------------------
# Return the string html unescaped.
#
    my $todecode = pop;
    return unless defined $todecode;
    if (ref $todecode eq 'SCALAR') {
        $$todecode =~ s/&lt;/</g;
        $$todecode =~ s/&gt;/>/g;
        $$todecode =~ s/&quot;/"/g;
        $$todecode =~ s/&#039;/'/g;
        $$todecode =~ s/&amp;/&/g;
    }
    else {
        $todecode =~ s/&lt;/</g;
        $todecode =~ s/&gt;/>/g;
        $todecode =~ s/&quot;/"/g;
        $todecode =~ s/&#039;/'/g;
        $todecode =~ s/&amp;/&/g;
    }
    return $todecode;
}
END_OF_SUB

$COMPILE{self_url} = __LINE__ . <<'END_OF_SUB';
sub self_url {
# -------------------------------------------------------------------
# Return full URL with query options as CGI.pm
#
    return $_[0]->url(query_string => 1, absolute => 1);
}
END_OF_SUB

$COMPILE{url} = __LINE__ . <<'END_OF_SUB';
sub url {
# -------------------------------------------------------------------
# Return the current url. Can be called as GT::CGI->url() or $cgi->url().
#
    my $self = shift;
    $self = $self->class_new unless ref $self;
    $self->load_data() unless $self->{data_loaded};
    my $opts = $self->common_param(@_);

    my $absolute        = exists $opts->{absolute} ? $opts->{absolute} : 0;
    my $query_string    = exists $opts->{query_string} ? $opts->{query_string} : 1;
    my $path_info       = exists $opts->{path_info}    ? $opts->{path_info} : 0;
    my $remove_empty    = exists $opts->{remove_empty} ? $opts->{remove_empty} : 0;
    if ($opts->{relative}) {
        $absolute = 0;
    }

    my $url = '';
    my $script = $ENV{SCRIPT_NAME} || $0;
    my ($path, $prog) = $script =~ m,^(.+?)[/\\]?([^/\\]*)$,;

    if ($absolute) {
        my ($protocol, $version) = split('/', $ENV{SERVER_PROTOCOL} || 'HTTP/1.0');
        $url = lc $protocol . "://";

        my $host = $ENV{HTTP_HOST} || $ENV{SERVER_NAME} || '';
        $url .= $host;

        $path =~ s{^[/\\]+}{};
        $path =~ s{[/\\]+$}{};
        $url .= "/$path/";
    }
    $prog =~ s{^[/\\]+}{};
    $prog =~ s{[/\\]+$}{};
    $url .= $prog;

    if ($path_info and $ENV{PATH_INFO}) {
        my $path = $ENV{PATH_INFO};
        if (defined $ENV{SERVER_SOFTWARE} && $ENV{SERVER_SOFTWARE} =~ /IIS/) {
            $path =~ s/\Q$ENV{SCRIPT_NAME}//;
        }
        $url .= $path;
    }
    if ($query_string) {
        my $qs = $self->query_string(remove_empty => $remove_empty);
        if ($qs) {
            $url .= "?" . $qs;
        }
    }
    return $url;
}
END_OF_SUB

$COMPILE{query_string} = __LINE__ . <<'END_OF_SUB';
sub query_string {
# -------------------------------------------------------------------
# Returns the query string url escaped.
#
    my $self = shift;
    $self = $self->class_new unless ref $self;
    $self->load_data() unless $self->{data_loaded};
    my $opts = $self->common_param(@_);
    my $qs = '';
    foreach my $key (@{$self->{param_order} || []}) {
        my $esc_key = escape($key);
        foreach my $val (@{$self->{params}->{$key}}) {
            next if ($opts->{remove_empty} and ($val eq ''));
            $qs .= $esc_key . "=" . escape($val) . ";";
        }
    }
    $qs and chop $qs;
    $qs ? return $qs : return '';
}
END_OF_SUB

$COMPILE{post_data} = __LINE__ . <<'END_OF_SUB';
sub post_data {
# -------------------------------------------------------------------
# Returns the POSTed data if it was not of type
# application/x-www-form-urlencoded or multipart/form-data.
#
    my $self = shift;
    $self = $self->class_new unless ref $self;
    $self->load_data() unless $self->{data_loaded};

    return $self->{post_data};
}
END_OF_SUB

$COMPILE{browser_info} = __LINE__ . <<'END_OF_SUB';
sub browser_info {
# -----------------------------------------------------------------------------
# my %tags = browser_info();
# --------------------------
#   Returns various is_BROWSER, BROWSER_version tags.
#
    return unless $ENV{HTTP_USER_AGENT};

    my %browser_opts;

    if ($ENV{HTTP_USER_AGENT} =~ m{Opera(?:\s+|/)(\d+\.\d+)}i) {
        $browser_opts{is_opera} = 1;
        $browser_opts{opera_version} = $1;
    }
    elsif ($ENV{HTTP_USER_AGENT} =~ /MSIE (\d+(?:\.\d+)?)/i) {
        $browser_opts{is_ie} = 1;
        $browser_opts{ie_version} = $1;
    }
    elsif ($ENV{HTTP_USER_AGENT} =~ m{Mozilla/(\d+\.\d+)\s+\(.*\s+rv:(\d+\.\d+)}i) {
        if ($1 >= 5.0) {
            $browser_opts{is_mozilla} = 1;
            $browser_opts{mozilla_version} = $2;
        }
    }
    elsif ($ENV{HTTP_USER_AGENT} =~ m{Safari/(\d+(?:\.\d+)?)}i) {
        $browser_opts{is_safari} = 1;
        $browser_opts{safari_version} = $1;
    }
    elsif ($ENV{HTTP_USER_AGENT} =~ m{Konqueror/(\d+\.\d+)}i) {
        $browser_opts{is_konqueror} = 1;
        $browser_opts{konqueror_version} = $1;
    }
    return %browser_opts;
}
END_OF_SUB

$COMPILE{upload_hook} = __LINE__ . <<'END_OF_SUB';
sub upload_hook {
#--------------------------------------------------------------------------------
# Provides a hook to access file upload data while it is being read from client.
# 
    my $self = shift;
    $self->{upload_hook} = shift if @_;
    return $self->{upload_hook};
}
END_OF_SUB

sub parse_str {
#--------------------------------------------------------------------------------
# parses a query string and add it to the parameter list
#
    my $self = shift;
    my @input;
    for (split /[;&]/, shift) {
        my ($key, $val) = /([^=]+)=(.*)/ or next;

# Re-taint the CGI input
        $key .= $TAINTED;
        $val .= $TAINTED;

# Need to remove cr's on windows.
        if ($^O eq 'MSWin32') {
            $key =~ s/%0D%0A/%0A/gi; # \x0d = \r, \x0a = \n
            $val =~ s/%0D%0A/%0A/gi;
        }
        push @input, $key, $val;
    }
    unescape(\@input);
    while (@input) {
        my ($k, $v) = splice @input, 0, 2;
        $self->{params}->{$k} or push @{$self->{param_order}}, $k;
        unshift @{$self->{params}->{$k}}, $v;
    }
}

1;


} # End of BEGIN for GT/CGI.pm

BEGIN {
    $INC{"GT/CGI/Cookie.pm"} = "GT/CGI/Cookie.pm";

# ==================================================================
# Gossamer Threads Module Library - http://gossamer-threads.com/
#
#   GT::CGI::Cookie
#   $Id: Cookie.pm,v 1.7 2008/06/09 23:39:47 brewt Exp $
# 
# Copyright (c) 2004 Gossamer Threads Inc.  All Rights Reserved.
# ==================================================================
#
# Description:
#   Handles cookie creation and formatting
#

package GT::CGI::Cookie;
#================================================================================

use strict;
use GT::CGI;
use GT::Base;
use vars qw/@ISA $ATTRIBS @MON @WDAY/;

@ISA = qw/GT::Base/;

$ATTRIBS = {
    -name     => '',
    -value    => '',
    -expires  => '',
    -path     => '',
    -domain   => '',
    -secure   => '',
    -httponly => '',
};
@MON  = qw/Jan Feb Mar Apr May Jun Jul Aug Sep Oct Nov Dec/;
@WDAY = qw/Sun Mon Tue Wed Thu Fri Sat/;

sub cookie_header {
#--------------------------------------------------------------------------------
# Returns a cookie header.
#
    my $self    = shift;

# make sure we have a name to use
    $self->{-name} or return;

    my $name  = GT::CGI::escape($self->{-name});
    my $value = GT::CGI::escape($self->{-value});

# build the header that creates the cookie
    my $header = "Set-Cookie: $name=$value";

    $self->{-expires} and $header .= "; expires=" . $self->format_date('-', $self->{-expires});
    if (my $path = $self->{-path}) { $path =~ s/[\x00-\x1f].*//s; $header .= "; path=$path"; }
    if (my $domain = $self->{-domain}) { $domain =~ s/[\x00-\x1f].*//s; $header .= "; domain=$domain"; }
    $self->{-secure}  and $header .= "; secure";
    $self->{-httponly} and $header .= "; httponly";

    return $header;
}

sub format_date {
# -------------------------------------------------------------------
# Returns a string in http_gmt format, but accepts one in unknown format.
#   Wed, 23 Aug 2000 21:20:14 GMT
#
    my ($self, $sep, $datestr) = @_;
    my $unix_time = defined $datestr ? $self->expire_calc($datestr) : time;

    my ($sec, $min, $hour, $mday, $mon, $year, $wday) = gmtime($unix_time);
    $year += 1900;

    return sprintf(
        "%s, %02d$sep%s$sep%04d %02d:%02d:%02d GMT",
        $WDAY[$wday], $mday, $MON[$mon], $year, $hour, $min, $sec
    );
}
*_format_date = \&format_date; # deprecated

sub expire_calc {
# -------------------------------------------------------------------
# Calculates when a date based on +- times. See CGI.pm for more info.
#
    my ($self, $time) = @_;
    my %mult = (s => 1, m => 60, h => 3600, d => 86400, M => 2592000, y => 31536000);
    my $offset;

    if (!$time or lc $time eq 'now') {
        $offset = 0;
    }
    elsif ($time =~ /^\d/) {
        return $time;
    }
    elsif ($time=~/^([+-]?(?:\d+(?:\.\d*)?|\.\d+))([smhdMy]?)/) {
        $offset = $1 * ($mult{$2} || 1);
    }
    else {
        return $time;
    }
    return time + $offset;
}
*_expire_calc = \&expire_calc; # deprecated

1;

} # End of BEGIN for GT/CGI/Cookie.pm

BEGIN {
    $INC{"GT/MD5.pm"} = "GT/MD5.pm";

# ==================================================================
# Gossamer Threads Module Library - http://gossamer-threads.com/
#
#   GT::MD5
#   Author: Scott Beck (see pod for details)
#   $Id: MD5.pm,v 1.19 2004/11/17 01:23:30 jagerman Exp $
#
# Copyright (c) 2004 Gossamer Threads Inc.  All Rights Reserved.
# See bottom for addition Copyrights.
# ==================================================================
#
# Description: This is an implementation of the MD5 algorithm in perl.
#

package GT::MD5;
# ==================================================================
use strict;
use vars qw($VERSION @ISA @EXPORTER @EXPORT_OK $DATA);

@EXPORT_OK = qw(md5 md5_hex md5_base64);

@ISA = qw(Exporter);
$VERSION = sprintf "%d.%03d", q$Revision: 1.19 $ =~ /(\d+)\.(\d+)/;

$DATA = <<'END_OF_CODE';
use integer;

# I-Vektor
sub A() { 0x67_45_23_01 }
sub B() { 0xef_cd_ab_89 }
sub C() { 0x98_ba_dc_fe }
sub D() { 0x10_32_54_76 }

# for internal use
sub MAX() { 0xFFFFFFFF }

@GT::MD5::DATA = split "\n", q|
FF,$a,$b,$c,$d,$_[4],7,0xd76aa478,/* 1 */
FF,$d,$a,$b,$c,$_[5],12,0xe8c7b756,/* 2 */
FF,$c,$d,$a,$b,$_[6],17,0x242070db,/* 3 */
FF,$b,$c,$d,$a,$_[7],22,0xc1bdceee,/* 4 */
FF,$a,$b,$c,$d,$_[8],7,0xf57c0faf,/* 5 */
FF,$d,$a,$b,$c,$_[9],12,0x4787c62a,/* 6 */
FF,$c,$d,$a,$b,$_[10],17,0xa8304613,/* 7 */
FF,$b,$c,$d,$a,$_[11],22,0xfd469501,/* 8 */
FF,$a,$b,$c,$d,$_[12],7,0x698098d8,/* 9 */
FF,$d,$a,$b,$c,$_[13],12,0x8b44f7af,/* 10 */
FF,$c,$d,$a,$b,$_[14],17,0xffff5bb1,/* 11 */
FF,$b,$c,$d,$a,$_[15],22,0x895cd7be,/* 12 */
FF,$a,$b,$c,$d,$_[16],7,0x6b901122,/* 13 */
FF,$d,$a,$b,$c,$_[17],12,0xfd987193,/* 14 */
FF,$c,$d,$a,$b,$_[18],17,0xa679438e,/* 15 */
FF,$b,$c,$d,$a,$_[19],22,0x49b40821,/* 16 */ 
GG,$a,$b,$c,$d,$_[5],5,0xf61e2562,/* 17 */
GG,$d,$a,$b,$c,$_[10],9,0xc040b340,/* 18 */
GG,$c,$d,$a,$b,$_[15],14,0x265e5a51,/* 19 */
GG,$b,$c,$d,$a,$_[4],20,0xe9b6c7aa,/* 20 */
GG,$a,$b,$c,$d,$_[9],5,0xd62f105d,/* 21 */
GG,$d,$a,$b,$c,$_[14],9,0x2441453,/* 22 */
GG,$c,$d,$a,$b,$_[19],14,0xd8a1e681,/* 23 */
GG,$b,$c,$d,$a,$_[8],20,0xe7d3fbc8,/* 24 */
GG,$a,$b,$c,$d,$_[13],5,0x21e1cde6,/* 25 */
GG,$d,$a,$b,$c,$_[18],9,0xc33707d6,/* 26 */
GG,$c,$d,$a,$b,$_[7],14,0xf4d50d87,/* 27 */
GG,$b,$c,$d,$a,$_[12],20,0x455a14ed,/* 28 */
GG,$a,$b,$c,$d,$_[17],5,0xa9e3e905,/* 29 */
GG,$d,$a,$b,$c,$_[6],9,0xfcefa3f8,/* 30 */
GG,$c,$d,$a,$b,$_[11],14,0x676f02d9,/* 31 */
GG,$b,$c,$d,$a,$_[16],20,0x8d2a4c8a,/* 32 */
HH,$a,$b,$c,$d,$_[9],4,0xfffa3942,/* 33 */
HH,$d,$a,$b,$c,$_[12],11,0x8771f681,/* 34 */
HH,$c,$d,$a,$b,$_[15],16,0x6d9d6122,/* 35 */
HH,$b,$c,$d,$a,$_[18],23,0xfde5380c,/* 36 */
HH,$a,$b,$c,$d,$_[5],4,0xa4beea44,/* 37 */
HH,$d,$a,$b,$c,$_[8],11,0x4bdecfa9,/* 38 */
HH,$c,$d,$a,$b,$_[11],16,0xf6bb4b60,/* 39 */
HH,$b,$c,$d,$a,$_[14],23,0xbebfbc70,/* 40 */
HH,$a,$b,$c,$d,$_[17],4,0x289b7ec6,/* 41 */
HH,$d,$a,$b,$c,$_[4],11,0xeaa127fa,/* 42 */
HH,$c,$d,$a,$b,$_[7],16,0xd4ef3085,/* 43 */
HH,$b,$c,$d,$a,$_[10],23,0x4881d05,/* 44 */
HH,$a,$b,$c,$d,$_[13],4,0xd9d4d039,/* 45 */
HH,$d,$a,$b,$c,$_[16],11,0xe6db99e5,/* 46 */
HH,$c,$d,$a,$b,$_[19],16,0x1fa27cf8,/* 47 */
HH,$b,$c,$d,$a,$_[6],23,0xc4ac5665,/* 48 */
II,$a,$b,$c,$d,$_[4],6,0xf4292244,/* 49 */
II,$d,$a,$b,$c,$_[11],10,0x432aff97,/* 50 */
II,$c,$d,$a,$b,$_[18],15,0xab9423a7,/* 51 */
II,$b,$c,$d,$a,$_[9],21,0xfc93a039,/* 52 */
II,$a,$b,$c,$d,$_[16],6,0x655b59c3,/* 53 */
II,$d,$a,$b,$c,$_[7],10,0x8f0ccc92,/* 54 */
II,$c,$d,$a,$b,$_[14],15,0xffeff47d,/* 55 */
II,$b,$c,$d,$a,$_[5],21,0x85845dd1,/* 56 */
II,$a,$b,$c,$d,$_[12],6,0x6fa87e4f,/* 57 */
II,$d,$a,$b,$c,$_[19],10,0xfe2ce6e0,/* 58 */
II,$c,$d,$a,$b,$_[10],15,0xa3014314,/* 59 */
II,$b,$c,$d,$a,$_[17],21,0x4e0811a1,/* 60 */
II,$a,$b,$c,$d,$_[8],6,0xf7537e82,/* 61 */
II,$d,$a,$b,$c,$_[15],10,0xbd3af235,/* 62 */
II,$c,$d,$a,$b,$_[6],15,0x2ad7d2bb,/* 63 */
II,$b,$c,$d,$a,$_[13],21,0xeb86d391,/* 64 */|;


# padd a message to a multiple of 64
sub padding {
    my $l = length (my $msg = shift() . chr(128));    
    $msg .= "\0" x (($l%64<=56?56:120)-$l%64);
    $l = ($l-1)*8;
    $msg .= pack 'VV', $l & MAX , ($l >> 16 >> 16);
}


sub rotate_left($$) {
	#$_[0] << $_[1] | $_[0] >> (32 - $_[1]);
	#my $right = $_[0] >> (32 - $_[1]);
	#my $rmask = (1 << $_[1]) - 1;
	($_[0] << $_[1]) | (( $_[0] >> (32 - $_[1])  )  & ((1 << $_[1]) - 1));
	#$_[0] << $_[1] | (($_[0]>> (32 - $_[1])) & (1 << (32 - $_[1])) - 1);
}

sub gen_code {
  # Discard upper 32 bits on 64 bit archs.
  my $MSK = ((1 << 16) << 16) ? ' & ' . MAX : '';
#	FF => "X0=rotate_left(((X1&X2)|(~X1&X3))+X0+X4+X6$MSK,X5)+X1$MSK;",
#	GG => "X0=rotate_left(((X1&X3)|(X2&(~X3)))+X0+X4+X6$MSK,X5)+X1$MSK;",
  my %f = (
	FF => "X0=rotate_left((X3^(X1&(X2^X3)))+X0+X4+X6$MSK,X5)+X1$MSK;",
	GG => "X0=rotate_left((X2^(X3&(X1^X2)))+X0+X4+X6$MSK,X5)+X1$MSK;",
	HH => "X0=rotate_left((X1^X2^X3)+X0+X4+X6$MSK,X5)+X1$MSK;",
	II => "X0=rotate_left((X2^(X1|(~X3)))+X0+X4+X6$MSK,X5)+X1$MSK;",
  );
  #unless ( (1 << 16) << 16) { %f = %{$CODES{'32bit'}} }
  #else { %f = %{$CODES{'64bit'}} }

  my %s = (  # shift lengths
	S11 => 7, S12 => 12, S13 => 17, S14 => 22, S21 => 5, S22 => 9, S23 => 14,
	S24 => 20, S31 => 4, S32 => 11, S33 => 16, S34 => 23, S41 => 6, S42 => 10,
	S43 => 15, S44 => 21
  );

  my $insert = "\n";
#  while(<DATA>) {
  for (@GT::MD5::DATA) {
#	chomp;
	next unless /^[FGHI]/;
	my ($func,@x) = split /,/;
	my $c = $f{$func};
	$c =~ s/X(\d)/$x[$1]/g;
	$c =~ s/(S\d{2})/$s{$1}/;
	$c =~ s/^(.*)=rotate_left\((.*),(.*)\)\+(.*)$//;

	my $su = 32 - $3;
	my $sh = (1 << $3) - 1;

	$c = "$1=(((\$r=$2)<<$3)|((\$r>>$su)&$sh))+$4";

	#my $rotate = "(($2 << $3) || (($2 >> (32 - $3)) & (1 << $2) - 1)))"; 
	# $c = "\$r = $2;
	# $1 = ((\$r << $3) | ((\$r >> (32 - $3))  & ((1 << $3) - 1))) + $4";
	$insert .= "\t$c\n";
  }
#  close DATA;
  
  my $dump = '
  sub round {
	my ($a,$b,$c,$d) = @_[0 .. 3];
	my $r;' . $insert . '
	$_[0]+$a' . $MSK . ', $_[1]+$b ' . $MSK . 
        ', $_[2]+$c' . $MSK . ', $_[3]+$d' . $MSK . ';
  }';
  eval $dump;
  # print "$dump\n";
  # exit 0;
}

gen_code();

#########################################
# Private output converter functions:
sub _encode_hex { unpack 'H*', $_[0] }
sub _encode_base64 {
	my $res;
	while ($_[0] =~ /(.{1,45})/gs) {
		$res .= substr pack('u', $1), 1;
		chop $res;
	}
	$res =~ tr|` -_|AA-Za-z0-9+/|;#`
	chop $res; chop $res;
	$res
}

#########################################
# OOP interface:
sub new {
	my $proto = shift;
	my $class = ref $proto || $proto;
	my $self = {};
	bless $self, $class;
	$self->reset();
	$self
}

sub reset {
	my $self = shift;
	delete $self->{_data};
	$self->{_state} = [A,B,C,D];
	$self->{_length} = 0;
	$self
}

sub add {
	my $self = shift;
	$self->{_data} .= join '', @_ if @_;
	my ($i,$c);
	for $i (0 .. (length $self->{_data})/64-1) {
		my @X = unpack 'V16', substr $self->{_data}, $i*64, 64;
		@{$self->{_state}} = round(@{$self->{_state}},@X);
		++$c;
	}
	if ($c) {
		substr ($self->{_data}, 0, $c*64) = '';
		$self->{_length} += $c*64;
	}
	$self
}

sub finalize {
	my $self = shift;
	$self->{_data} .= chr(128);
    my $l = $self->{_length} + length $self->{_data};
    $self->{_data} .= "\0" x (($l%64<=56?56:120)-$l%64);
    $l = ($l-1)*8;
    $self->{_data} .= pack 'VV', $l & MAX , ($l >> 16 >> 16);
	$self->add();
	$self
}

sub addfile {
  	my ($self,$fh) = @_;
	if (!ref($fh) && ref(\$fh) ne "GLOB") {
	    require Symbol;
	    $fh = Symbol::qualify($fh, scalar caller);
	}
	# $self->{_data} .= do{local$/;<$fh>};
	my $read = 0;
	my $buffer = '';
	$self->add($buffer) while $read = read $fh, $buffer, 8192;
	die "GT::MD5 read failed: $!" unless defined $read;
	$self
}

sub add_bits {
	my $self = shift;
	return $self->add( pack 'B*', shift ) if @_ == 1;
	my ($b,$n) = @_;
	die "GT::MD5 Invalid number of bits\n" if $n%8;
	$self->add( substr $b, 0, $n/8 )
}

sub digest {
	my $self = shift;
	$self->finalize();
	my $res = pack 'V4', @{$self->{_state}};
	$self->reset();
	$res
}

sub hexdigest {
	_encode_hex($_[0]->digest)
}

sub b64digest {
	_encode_base64($_[0]->digest)
}

sub clone {
	my $self = shift;
	my $clone = { 
		_state => [@{$self->{_state}}],
		_length => $self->{_length},
		_data => $self->{_data}
	};
	bless $clone, ref $self || $self;
}

#########################################
# Procedural interface:
sub md5 {
	my $message = padding(join'',@_);
	my ($a,$b,$c,$d) = (A,B,C,D);
	my $i;
	for $i (0 .. (length $message)/64-1) {
		my @X = unpack 'V16', substr $message,$i*64,64;	
		($a,$b,$c,$d) = round($a,$b,$c,$d,@X);
	}
	pack 'V4',$a,$b,$c,$d;
}
sub md5_hex { _encode_hex &md5 }
sub md5_base64 { _encode_base64 &md5 }
END_OF_CODE

# Load either Digest::MD5 or GT::MD5 functions.
eval {
    local $SIG{__DIE__};
    require Digest::MD5;
    foreach (@EXPORT_OK) { delete $GT::MD5::{$_}; } # Do not remove.
    import Digest::MD5 (@EXPORT_OK);
    *GT::MD5::md5_hex = sub { &Digest::MD5::md5_hex };
    *GT::MD5::md5 = sub { &Digest::MD5::md5 };
    *GT::MD5::md5_base64 = sub { &Digest::MD5::md5_base64 };
    @ISA = 'Digest::MD5';
    1;
}
or do {
    local $@;
    eval $DATA;
    $@ and die "GT::MD5 => can't compile: $@";
};

require Exporter;
import Exporter;

1;


} # End of BEGIN for GT/MD5.pm

BEGIN {
    $INC{"GT/MD5/Crypt.pm"} = "GT/MD5/Crypt.pm";

# GT::MD5::Crypt - adapted from CPAN Crypt::PasswdMD5 for use in the
# Gossamer Thread module library. gt_md5_crypt was added which uses
# "$GT$" as the magic string instead of the unix "$1$" or apache "$apr1$"
#
# Crypt::PasswdMD5: Module to provide an interoperable crypt() 
#       function for modern Unix O/S. This is based on the code for
#
# /usr/src/libcrypt/crypt.c
#
# on a FreeBSD 2.2.5-RELEASE system, which included the following
# notice.
#
# ----------------------------------------------------------------------------
# "THE BEER-WARE LICENSE" (Revision 42):
# <phk@login.dknet.dk> wrote this file.  As long as you retain this notice you
# can do whatever you want with this stuff. If we meet some day, and you think
# this stuff is worth it, you can buy me a beer in return.   Poul-Henning Kamp
# ----------------------------------------------------------------------------
#
# 19980710 lem@cantv.net: Initial release
# 19990402 bryan@eai.com: Added apache_md5_crypt to create a valid hash
#                         for use in .htpasswd files
# 20001006 wrowe@lnd.com: Requested apache_md5_crypt to be
#                         exported by default.
#
################

package GT::MD5::Crypt;
$VERSION='1.1';
require 5.000;
require Exporter;
@ISA = qw(Exporter);
@EXPORT = qw(unix_md5_crypt apache_md5_crypt gt_md5_crypt);


$Magic = '$1$'; # Magic string
$itoa64 = "./0123456789ABCDEFGHIJKLMNOPQRSTUVWXYZabcdefghijklmnopqrstuvwxyz";

local $^W;

use GT::MD5;

sub to64 {
    my ($v, $n) = @_;
    my $ret = '';
    while (--$n >= 0) {
        $ret .= substr($itoa64, $v & 0x3f, 1);
        $v >>= 6;
    }
    $ret;
}

sub apache_md5_crypt {
    # change the Magic string to match the one used by Apache
    local $Magic = '$apr1$';

    unix_md5_crypt(@_);
}

sub gt_md5_crypt {
    # change the Magic string to put our signature in the password
    local $Magic = '$GT$';

    unix_md5_crypt(@_);
}

sub unix_md5_crypt {
    my($pw, $salt) = @_;
    my $passwd;

    $salt =~ s/^\Q$Magic//;     # Take care of the magic string if
                                # if present.

    $salt =~ s/^(.*)\$.*$/$1/;  # Salt can have up to 8 chars...
    $salt = substr($salt, 0, 8);

    $ctx = new GT::MD5;         # Here we start the calculation
    $ctx->add($pw);             # Original password...
    $ctx->add($Magic);          # ...our magic string...
    $ctx->add($salt);           # ...the salt...

    my ($final) = new GT::MD5;
    $final->add($pw);
    $final->add($salt);
    $final->add($pw);
    $final = $final->digest;

    for ($pl = length($pw); $pl > 0; $pl -= 16) {
        $ctx->add(substr($final, 0, $pl > 16 ? 16 : $pl));
    }

                                # Now the 'weird' xform

    for ($i = length($pw); $i; $i >>= 1) {
        if ($i & 1) { $ctx->add(pack("C", 0)); }
                                # This comes from the original version,
                                # where a memset() is done to $final
                                # before this loop.
        else { $ctx->add(substr($pw, 0, 1)); }
    }

    $final = $ctx->digest;
                                # The following is supposed to make
                                # things run slower. In perl, perhaps
                                # it'll be *really* slow!

    for ($i = 0; $i < 1000; $i++) {
        $ctx1 = new GT::MD5;
        if ($i & 1) { $ctx1->add($pw); }
        else { $ctx1->add(substr($final, 0, 16)); }
        if ($i % 3) { $ctx1->add($salt); }
        if ($i % 7) { $ctx1->add($pw); }
        if ($i & 1) { $ctx1->add(substr($final, 0, 16)); }
        else { $ctx1->add($pw); }
        $final = $ctx1->digest;
    }
    
                                # Final xform

    $passwd = '';
    $passwd .= to64(int(unpack("C", (substr($final, 0, 1))) << 16)
                    | int(unpack("C", (substr($final, 6, 1))) << 8)
                    | int(unpack("C", (substr($final, 12, 1)))), 4);
    $passwd .= to64(int(unpack("C", (substr($final, 1, 1))) << 16)
                    | int(unpack("C", (substr($final, 7, 1))) << 8)
                    | int(unpack("C", (substr($final, 13, 1)))), 4);
    $passwd .= to64(int(unpack("C", (substr($final, 2, 1))) << 16)
                    | int(unpack("C", (substr($final, 8, 1))) << 8)
                    | int(unpack("C", (substr($final, 14, 1)))), 4);
    $passwd .= to64(int(unpack("C", (substr($final, 3, 1))) << 16)
                    | int(unpack("C", (substr($final, 9, 1))) << 8)
                    | int(unpack("C", (substr($final, 15, 1)))), 4);
    $passwd .= to64(int(unpack("C", (substr($final, 4, 1))) << 16)
                    | int(unpack("C", (substr($final, 10, 1))) << 8)
                    | int(unpack("C", (substr($final, 5, 1)))), 4);
    $passwd .= to64(int(unpack("C", substr($final, 11, 1))), 2);

    $final = '';
    $Magic . $salt . '$' . $passwd;
}

1;


} # End of BEGIN for GT/MD5/Crypt.pm

BEGIN {
    $INC{"GT/Installer.pm"} = "GT/Installer.pm";

# ==================================================================
# Gossamer Threads Module Library - http://gossamer-threads.com/
#
#   GT::Installer
#   Author  : Scott Beck
#   $Id: Installer.pm,v 1.98 2008/11/18 03:10:03 brewt Exp $
#
# Copyright (c) 2004 Gossamer Threads Inc.  All Rights Reserved.
# ==================================================================
#
# Description:
#   Handle GT product installs.
#

package GT::Installer;
# ===============================================================

use strict qw/vars refs/;
use vars qw/
    @ISA
    $ERRORS
    $ATTRIBS
    $USE_LIB_SPACES
    $REMOVE_INSTALL
    $Error_Breakout
/;

$ATTRIBS = {
    install_to           => {},
    lite                 => 0,
    product              => undef,
    version              => undef,
    checksums            => undef,
    tar_checksum         => undef,
    untar_callback       => undef,
    load_defaults        => undef,
    load_config          => undef,
    save_config          => undef,
    prompts              => [],
    config               => {},
    defaults             => {},
    upgrade              => [],
    use_lib              => 'Admin Path',
    replace_path         => undef,
    use_init             => undef,
    install_exit_message => undef,
    upgrade_exit_message => undef,
    checksum_regex       => '\.(?:pm|pl|cgi|html?)$',
    fixup_regex          => '\.(?:cgi|pl)$',
    initial_message      => [],
    welcome_format       => 'plain',
    header_format        => 'professional',
    perl_flags           => '',
    save_cache           => 1
};

$ERRORS = {
    REQUIRED   => "%s cannot be left blank.",
    PATH       => "The path (%s) does not exist on this system",
    PATHWRITE  => "Unable to write to directory (%s): %s",
    PATHCREATE => "Unable to create directory (%s): %s",
    URLFMT     => "(%s) does not look like a URL",
    FTPFMT     => "(%s) does not look like and FTP URL",
    EMAILFMT   => "(%s) does not look like an e-mail address",
    SENDMAIL   => "The path (%s) does not exist on your system or is not executable",
    SMTP       => "(%s) is not a valid SMTP server address",
    PERL       => "The path to perl you specified (%s) %s",
    DIREXISTS  => "%s is not a directory but exists; unable to make a directory of that name",
};
$USE_LIB_SPACES = "";
$REMOVE_INSTALL = 0;

@ISA = qw(GT::Base);
import GT::MD5 qw/md5_hex/;

sub CHECK_BYTES () { 10240 }

sub init {
    my $self = shift;

    for (keys %{$ERRORS}) {
        if (exists $GT::Installer::LANG{"ERR_$_"}) {
            $ERRORS->{$_} = $GT::Installer::LANG{"ERR_$_"};
        }
    }
    for (keys %{$GT::Tar::ERRORS}) {
        if (exists $GT::Installer::LANG{"TAR_$_"}) {
            $GT::Tar::ERRORS->{$_} = $GT::Installer::LANG{"TAR_$_"};
        }
    }

    $self->set(@_);
    $self->{is_cgi} = exists($ENV{REQUEST_METHOD});
    if ($self->{is_cgi}) {
        $self->{in} = new GT::CGI;
        if ($self->{in}->param('lite')) {
            $self->{lite} = 1;
        }
    }
    else {
        if (@ARGV and ($ARGV[0] eq '--lite')) {
            $self->{lite} = 1;
        }
    }
    return $self;
}

#################################################################
#                      User Prompt sets                         #
#################################################################
sub add_config {
# ---------------------------------------------------------------
# path, url, email
#
    my $self = shift;
    my $opts = $self->common_param(@_);
    $opts->{type}              or $self->error('BADARGS', 'FATAL', 'You must specify the type of prompt');

    if ($opts->{type} eq 'reg_number') {
        $opts->{message} ||= lang('enter_reg');
        defined($opts->{required}) or $opts->{required} = 0;
        $opts->{key}     ||= $opts->{message};
    }
    elsif ($opts->{type} eq 'email_support') {
        $opts->{message} ||= lang('enter_sendmail');
        $opts->{key}     ||= $opts->{message};
    }
    elsif ($opts->{type} eq 'perl_path') {
        $opts->{message} ||= lang('enter_perl');
        $opts->{key}     ||= $opts->{message};
    }
    elsif ($opts->{type} eq 'create_dirs') {
        $opts->{message} ||= lang('create_dirs');
        $opts->{key}     ||= $opts->{message};
    }
    defined($opts->{required}) or $opts->{required} = 1;
    $opts->{message} ||= $opts->{key};
    defined($opts->{key}) or $self->error('BADARGS', 'FATAL', 'You must specify what get\'s set');

    push @{$self->{prompts}}, $opts;
    return 1;
}

sub add_config_message {
# ---------------------------------------------------------------
# Add a configuration message.
#
    my ($self, $text, $format) = @_;
    defined($text) or $self->error('BADARGS', 'FATAL', 'You must specify the text for the message');
    $format ||= 'none';
    push @{$self->{prompts}}, {
        type    => 'message',
        message => $text,
        format  => $format,
    };
    return 1;
}

sub add_upgrade {
# ---------------------------------------------------------------
# Add an upgrade segment
#
    my $self = shift;
    my $opts = $self->common_param(@_);
    if (!$opts->{skip}) {
        $opts->{message} or $self->error('BADARGS', 'FATAL', 'You must specify the message to display');
        $opts->{format} ||= 'none';
        if (!defined($opts->{directory_list}) and !defined($opts->{file_list})) {
            $self->error('BADARGS', 'FATAL', 'You must specify one of file_list or directory_list');
        }
    }

    push @{$self->{upgrade}}, $opts;
    return 1;
}

sub initial_message {
# ---------------------------------------------------------------
# Add an upgrade message.
#
    my ($self, $text, $format) = @_;
    defined($text) or $self->error('BADARGS', 'FATAL', 'You must specify the text for the message');
    $format ||= 'none';
    $self->{initial_message}->[0] = $text;
    $self->{initial_message}->[1] = $format;
    return 1;
}

sub add_upgrade_message {
# ---------------------------------------------------------------
# Add an upgrade message.
#
    my ($self, $text, $format) = @_;
    defined($text) or $self->error('BADARGS', 'FATAL', 'You must specify the text for the message');
    $format ||= 'none';
    push @{$self->{upgrade}}, {
        type    => 'message',
        message => $text,
        format  => $format,
    };
    return 1;
}

sub install_to {
# ---------------------------------------------------------------
# Map regexs to path for installation.
#
    my $self = shift;
    my $opts = $self->common_param(@_) or return $self->{install_to};
    $self->{install_to} = $opts;
}

sub perform {
# ---------------------------------------------------------------
# Where it all begins. Should be called at the end of the main
# configuration routine.
#
    my ($self) = @_;

    $REMOVE_INSTALL = 0;
    $Error_Breakout = '';

# Make sure we are in our current directory.
    my $pwd = $self->find_cgi;
    chdir $pwd if $pwd;

# Set our sig handler to trap fatals, the 1 tells the sig handler
# not to exit.
    $SIG{__DIE__} = sub {
        die @_ if (GT::Base->in_eval());
        $self->disp_error(shift, 1);
    };

# Check perl version >= 5.004_04
    unless ($] >= 5.00404) {
        $self->disp_error(lang('install_version', $]));
    }

# First we check to see that the tar file macthes the checksum if there is a
# checksum
    if ($self->{tar_checksum}) {
        unless ($self->{lite}) {
            my $checksum = $self->get_checksum('./install.dat');
            if ($self->{tar_checksum} ne $checksum) {
                $self->disp_error(lang('install_currupted'));
            }
        }
    }
    if ($self->{is_cgi}) {
        $self->cgi_sequence;
    }
    else {
        $self->telnet_sequence;
        $REMOVE_INSTALL = 1;
    }
# Remove the files.
    if ($REMOVE_INSTALL) {
        unlink("install.dat");
        unlink("install.cgi");
    }
}

sub get_checksum {
# ---------------------------------------------------------------
# Given a path to the install.dat tar file returns a checksum
# of the first x bytes, where x is the constant CHECK_BYTES
#
    my ($self, $path) = @_;

    open FH, $path or $self->error('READOPEN', 'FATAL', $path, "$!");
    binmode FH;
    read(FH, my $buff, CHECK_BYTES);
    close FH;
    return md5_hex($buff);
}

sub disp_error {
# ---------------------------------------------------------------
# Displays the message passed in based on telnet or cgi and exits
#
    my ($self, $msg, $no_exit) = @_;
    if ($self->{is_cgi}) {
        print $self->{in}->header;
        $self->cgi_error($msg);
    }
    else {
        $self->telnet_error($msg);
    }
    exit unless ($no_exit);
}

sub cgi_sequence {
# ---------------------------------------------------------------
# Called internally if we are ran as a CGI.
#
    my ($self) = @_;

    print $self->{in}->header;
    if (!$self->{in}->param('accept_eula') and $self->{product} !~ /^cnn\s*radio/i) {
        $self->parse_print(html_eula => {});
    }
    elsif ($self->{in}->param('upgrade_choice') and not $self->{in}->param('cgi_first_screen')) {
        if ($self->{in}->param('upgrade_choice') eq 'Yes') {
            $self->{in}->param('install_dir') or return $self->cgi_first_screen(lang('admin_path_error'));
            $self->{config}->{GT_ADMIN_PATH} = $self->{in}->param('install_dir');
            $self->{upgrading} = 1;
            $self->{load_config}->($self) or return $self->cgi_first_screen($GT::Installer::error);
            $self->cgi_upgrade_first_screen;
        }
        else {
            $self->{installing} = 1;
            $self->{load_defaults}->($self);
            $self->cgi_install_first_screen;
        }
    }
    elsif ($self->{in}->param('upgrade_second')) {
        $self->{upgrading} = 1;
        $self->{config}->{GT_ADMIN_PATH} = $self->{in}->param('install_dir');
        $self->{load_config}->($self) or return $self->cgi_upgrade_first_screen($GT::Installer::error);
        $self->cgi_upgrade_second_screen;
    }
    elsif ($self->{in}->param('install')) {
        $self->{installing} = 1;
        $self->{load_defaults}->($self);
        $self->cgi_install_second_screen;
    }
    else {
        $self->cgi_first_screen;
    }
}

sub telnet_sequence {
# -------------------------------------------------------------------
# Initial telnet prompt
#
    my ($self) = @_;

    if ($ARGV[0] =~ /^-?(?:-d|-de|-def|-defa|-defau|-defaul|-default|-defaults)$/) {
        $self->{use_defaults} = 1;
    }
    eval {
        local $SIG{__DIE__};
        require Term::ReadLine;
        $self->{term} = Term::ReadLine->new('');
        $self->{term}->ornaments("aa,bb,cc,dd");
        *STDOUT = $self->out;
    };
    local $: = "\n ";

    $self->print(lang('intro', $self->{product}), $self->{header_format});

    my $res;
    if ($self->{product} !~ /^cnn\s*radio/i) {
        $self->parse_print(telnet_eula => {});
        $res = $self->telnet_prompt(lang('eula_prompt'), 'No', ['^Y(?:es)?$', '^No?$']);
        $res =~ /^y/i or return $self->disp_error(lang('eula_required'));
    }

    $self->print(lang('welcome', $self->{product}, $self->{product}), $self->{welcome_format});

    $self->print(@{$self->{initial_message}}) if @{$self->{initial_message}};
    $res = $self->telnet_prompt(lang('is_upgrade'), "No", ['^Y(?:es)?$', '^No?$']);
    if (lc($res) =~ /^y/) {
        $self->load_cache;
        $self->{config}->{GT_ADMIN_PATH} = $self->telnet_prompt(lang('enter_admin_path'), $self->{defaults}->{GT_ADMIN_PATH} );
        $self->save_cache;
        $self->{upgrading} = 1;
        if ($self->{load_config}) {
            $self->{load_config}->($self) or return $self->disp_error($GT::Installer::error);
        }
        $self->telnet_upgrade;
    }
    else {
        $self->{installing} = 1;
        if ($self->{load_defaults}) {
            $self->{load_defaults}->($self) or return $self->disp_error($GT::Installer::error);
        }
        $self->telnet_install;
    }
}

sub install {
# -------------------------------------------------------------------
# Install the program.
#
    my ($self) = @_;

    $self->{tar} ||= $self->_get_tar();
    my $files = $self->{tar}->files;

    $self->{untar_callback}->($self) if $self->{untar_callback};
    my %checksums;
    foreach my $file (@{$files}) {
        my $name = $file->name;
        my $mode = substr(sprintf("%lo", $file->mode), -3);
        my $type = $file->type;
        my $body;
        $body    = $file->body_as_string if $type == GT::Tar::FILE;
        foreach my $regex (keys %{$self->{install_to}}) {
            if ($name =~ /$regex/) {
                my $filename = $1;
                $file->name($self->{config}->{$self->{install_to}->{$regex}} . '/' . $filename);
                $self->_fixup_body($file, \$body) if ($filename =~ /(?:mod_perl\.pm|\.cgi|\.pl)$/ and $type == GT::Tar::FILE);
                last;
            }
        }

# Get a checksum if the it is a file and it matches
# the files we need to checksum.
        if ($type == GT::Tar::FILE) {
            if ($name =~ /$self->{checksum_regex}/) {
                $checksums{$name} = md5_hex($body) unless ($self->{lite});
            }
        }
# Don't chown the file.
        $file->set_owner(0);

# Untar the file
        print lang('unarchiving'), " ($mode) ", $file->name, "\n" if $type == GT::Tar::FILE;
        $file->write or return $self->disp_error($GT::Tar::error);
    }
    unless ($self->{lite}) {
        if ($self->{checksums}) {
            for (keys %{$self->{config}}) { $self->{checksums} =~ s/<%\Q$_\E%>/$self->{config}->{$_}/g }
            $self->recurse_mkdir($self->{checksum});
            my $fh = \do{ local *FH; *FH };
            if (open $fh, ">$self->{checksums}") {
                print {$fh} GT::Dumper->dump_structure(\%checksums);
            }
            else {
                $self->print(lang('err_writeopen', $self->{checksums}, $!), 'none');
            }
        }
    }
    if ($self->{save_config}) {
        $self->{save_config}->($self) or return $self->disp_error($GT::Installer::error);
    }
    return 1;
}

sub _fixup_body {
# -------------------------------------------------------------------
# Called internally to set the path to perl and the use lib line.
#
    my ($self, $file, $body) = @_;
    if ($self->{config}->{"Path to Perl"}) {
        if ($self->{perl_flags}) {
            $$body =~ s/^#![^\n\r]+\r?\n/#!$self->{config}->{"Path to Perl"} $self->{perl_flags}\n/;
        }
        else {
            $$body =~ s/^#![^\n\r]+\r?\n/#!$self->{config}->{"Path to Perl"}\n/;
        }
    }
    if ($self->{use_lib}) {
        for (keys %{$self->{config}}) { $self->{use_lib} =~ s/<%\Q$_\E%>/$self->{config}->{$_}/g }
        my $lib = $self->{use_lib};
        if (! ($$body =~ s/^\s*use\s*lib.+/${USE_LIB_SPACES}use lib '$lib';/m)) {
            $$body =~ s/(\s*use )/\n${USE_LIB_SPACES}use lib '$lib';\n$1/;
        }
    }
    if ($self->{use_init}) {
        my ($find, $replace) = @{$self->{use_init}};
        for (keys %{$self->{config}}) { $replace =~ s/<%\Q$_\E%>/$self->{config}->{$_}/g }
        $find  = $find . '::init';
        $$body =~ s/$find\(([^\)]*)\)/$find('$replace')/g;
    }
    if ($self->{replace_path}) {
        foreach my $key (keys %{$self->{replace_path}}) {
            my $value = $self->{replace_path}->{$key};
            for (keys %{$self->{config}}) { $value =~ s/<%\Q$_\E%>/$self->{config}->{$_}/g }
            $$body =~ s/$key/$value/g;
        }
    }
    $file->body($$body);
}

sub upgrade {
# -------------------------------------------------------------------
# Performs an upgrade.
#
    my ($self) = @_;

    $self->{tar} ||= $self->_get_tar();
    my $files = $self->{tar}->files;

    $self->{untar_callback}->($self) if $self->{untar_callback};
    my ($old_checksums, $new_checksums);

# Find the checksum file
    if ($self->{checksums}) {
        for (keys %{$self->{config}}) { $self->{checksums} =~ s/<%\Q$_\E%>/$self->{config}->{$_}/g }
    }
    if (-e $self->{checksums}) {
        $old_checksums = do $self->{checksums};
    }
    FILE: foreach my $file (@{$files}) {
        my $name = $file->name;
        my $mode = substr(sprintf("%lo", $file->mode), -3);
        my $type = $file->type;
        my $filename;
        foreach my $regex (keys %{$self->{install_to}}) {
            if ($name =~ /$regex/) {
                $filename = $1;
                $file->name($self->{config}->{$self->{install_to}->{$regex}} . '/' . $filename);
                last;
            }
        }

# Get a checksum if the it is a file and it matches
# the files we need to checksum. We need a checksum
# file for this as well.
        my $path = $file->name;
        CHECK: foreach my $rec (@{$self->{upgrade}}) {
            if (ref $rec->{skip} eq 'ARRAY' and @{$rec->{skip}}) {
                for (@{$rec->{skip}}) {
                    if ($name =~ /$_/ and -e $path) {
                        print lang('skipping_file', $path);
                        next FILE;
                    }
                }
            }
            elsif ($rec->{skip} and !ref $rec->{skip}) {
                if ($name =~ /$rec->{skip}/ and -e $path) {
                    print lang('skipping_file', $path);
                    next FILE;
                }
            }
        }
        if ($type == GT::Tar::FILE) {
            my $body = $file->body_as_string;
            $self->_fixup_body($file, \$body) if ($filename =~ /$self->{fixup_regex}/);

            unless ($self->{lite}) {
                if ($filename and $filename =~ /$self->{checksum_regex}/) {
                    if ($old_checksums and $old_checksums->{$name}) {
                        $self->_check_upgrade_files($old_checksums->{$name}, $path);
                    }
                    $new_checksums->{$name} = md5_hex($body);
                }
            }
        }

# Don't chown the file.
        $file->set_owner(0);

# Untar the file
        print lang('unarchiving'), " ($mode) $path\n" if $type == GT::Tar::FILE;

        $file->write or return $self->disp_error($GT::Tar::error);
        $file->body('');
    }
    if (! $self->{lite} and $self->{checksums}) {
        for (keys %{$self->{config}}) { $self->{checksums} =~ s/<%\Q$_\E%>/$self->{config}->{$_}/g }
        $self->recurse_mkdir($self->{checksums});
        my $fh = \do{ local *FH; *FH };
        if (open $fh, ">$self->{checksums}") {
            print {$fh} GT::Dumper->dump(var => '', data => $new_checksums);
        }
        else {
            $self->print(lang('err_writeopen', $self->{checksums}, $!), 'none');
        }
    }
    if ($self->{save_config}) {
        $self->{save_config}->($self) or return $self->disp_error($GT::Installer::error);
    }
    return 1;
}

sub _check_upgrade_files {
# -------------------------------------------------------------------
# Called internally to see what we should do with a file that matched
# an upgrade regex or path.
#
    my ($self, $checksum1, $path) = @_;
    my $fh = \do{ local *FH; *FH };
    open($fh, "< $path") or return;
    read($fh, my $buff, -s $fh);
    my $checksum2 = md5_hex($buff);
    if ($checksum1 ne $checksum2) {
        print lang('backing_up_file', $path);
        my $backup = "$path.bak";
        if (-e $backup) {
            my $i = 0;
            $backup = "$path$i.bak";
            until (!-e "$path$i.bak") {
                $i++;
                $backup = "$path$i.bak";
            }
        }
        rename($path, $backup) or print lang('err_rename', $path, $backup, $!);
    }
}

sub _get_tar {
# -------------------------------------------------------------------
# Does all error checking on tar open and returns the tar file 
# object.
#
    my ($self) = @_;
    $GT::Tar::error ||= ''; #silence -w
    my $tar = GT::Tar->open('install.dat');
    if (!$tar) {
        if ($GT::Tar::errcode eq 'CHECKSUM') {
            $self->print(lang('install_currupted'), 'none');
        }
        elsif ($GT::Tar::errcode eq 'OPEN') {
            $self->print(lang('err_opentar', $GT::Tar::error), 'none');
        }
        else {
            $self->print(lang('err_opentar_unknown', $GT::Tar::error), 'none');
        }
        exit;
    }
    return $tar;
}

sub format_validate {
# ---------------------------------------------------------------
# Formats and validates a specific kind of input. Takes the name
# of the field as the first argument and the value entered as the
# second argument. Returns false if valudation fails, returns the 
# formated return otherwise. If a last true argument is specified
# this method will return undef if path types does not pass -e
#
#
    my ($self, $set) = @_;
    $set ||= '';
    my $path_test;

    if ($self->{config}->{create_dirs}) {
        $path_test = 1;
    }

# First we need to look the record up for this set.
    my $rec;
    for (@{$self->{prompts}}) {
        next unless (ref($_) eq 'HASH');
        next unless ($_->{key});
        if ($set eq $_->{key}) {
            $rec = $_;
            last;
        }
    }
    (my $value = $self->{config}->{$rec->{key}}) =~ s/\r?\n//g;

# No value specified
    if (!$value) {
        if ($rec->{required}) {
            return $self->error('REQUIRED', 'WARN', $rec->{key});
        }
        $value = '';
    }

# Path type
    elsif ($rec->{type} eq 'path') {
        $value =~ s,/$,,;
        if ($path_test and !-e $value) {
            return $self->error('PATH', 'WARN', $value);
        }
        else {
            $self->recurse_mkdir("$value/") or return;
            if (! -e "$value/") {
                return $self->error('PATHCREATE', 'WARN', $value, "$!");
            }
            open(TEST, "> $value/tmp.txt") or return $self->error('PATHWRITE', 'WARN', $value, "$!");
            close TEST;
            unlink "$value/tmp.txt";
        }
    }

# URL type
    elsif ($rec->{type} eq 'url') {
        if ($value !~ m,^https?://.+,) {
            return $self->error('URLFMT', 'WARN', $value);
        }
        $value =~ s,/$,,;
    }

# URL type, allowing relative URL's
    elsif ($rec->{type} eq 'url_rel') {
        if ($value !~ m{^(?:https?://.|/)}) {
            return $self->error('URLFMT', 'WARN', $value);
        }
        $value =~ s,/$,,;
    }

# FTP type
    elsif ($rec->{type} eq 'ftp') {
        if ($value !~ m,^ftp://.+,) {
            return $self->error('FTPFMT', 'WARN', $value);
        }
        $value =~ s,/$,,;
    }

# email type
    elsif ($rec->{type} eq 'email') {
        if ($value !~ /.+\@.+\..+/) {
            return $self->error('EMAILFMT', 'WARN', $value);
        }
    }

# Reg num type
    elsif ($rec->{type} eq 'reg_number') {
    # FIXME: What to test here
    }
    elsif ($rec->{type} eq 'email_support') {

# Check the SMTP/Sendmail settings.
        if ($value =~ m,^/,) {
            if (! -x $value) {
                return $self->error('SENDMAIL', 'WARN', $value);
            }
            $self->{config}->{email_support} = 'sendmail';
        }
        # FIXME: Should I try and open a socket to this or atleast resolve the hostname?
        elsif ($value !~ /^[A-Za-z0-9\.\-]+$/) {
            return $self->error('SMTP', 'WARN', $value);
        }
        else {
            $self->{config}->{email_support} = 'smtp';
        }
    }

# Path to perl set
    elsif ($rec->{type} eq 'perl_path') {
        if (! -e $value) {
            return $self->error('PERL', 'WARN', $value, 'does not exist');
        }
        elsif (! -x _) {
            return $self->error('PERL', 'WARN', $value, 'is not executable');
        }
    }
    $self->{config}->{$rec->{key}} = $value;
    return 1;
}

#################################################################
#                 Upgrade/Install Telnet Prompts                #
#################################################################
sub telnet_upgrade {
# -------------------------------------------------------------------
# Performs a telnet upgrade.
#
    my ($self) = @_;
    PROMPTS: foreach my $rec (@{$self->{upgrade}}) {
        next if $rec->{skip};
        if ($rec->{type} eq 'message') {
            $self->print($rec->{message}, $rec->{format});
        }
        else {
            $self->print($rec->{message}, $rec->{format});
            $rec->{answer} = $self->telnet_prompt("Overwrite/Skip/Backup", 'Backup', ['^O(?:verwrite)?$', '^S(?:kip)?$', '^B(?:ackup)?$']);
        }
        if ($rec->{telnet_callback} and ref($rec->{telnet_callback}) eq 'CODE') {
            $rec->{telnet_callback}->($self) or redo PROMPTS;
        }
    }

# Check for any questions that still need to be asked as load_config couldn't handle them
    my @fields = @{$self->{prompts}};
    my $last_message;
    for my $rec (@fields) {
        if ($rec->{type} eq 'message') {
            $last_message = $rec;
            next;
        }
        if ($self->{need_upgrade_answer}->{$rec->{key}}) {
            PROMPT: {
                $self->print($last_message->{message}, $last_message->{format}) if $last_message;
                $self->load_cache;
                $self->telnet_prompt($rec->{message}, $self->{config}->{$rec->{key}} || $self->{defaults}->{$rec->{key}}, $rec);
                $self->save_cache;

                if ($rec->{telnet_callback} and ref($rec->{telnet_callback}) eq 'CODE') {
                    $rec->{telnet_callback}->($self) or redo PROMPT;
                }
            }
        }
        $last_message = undef;
    }

    $self->print(lang('we_have_it'), 'none');
    foreach my $rec (@{$self->{upgrade}}) {
        next if $rec->{type} eq 'message';
        next if not defined $self->{config}->{$rec->{key}} or not length $self->{config}->{$rec->{key}};
        local ($a, $b) = ($rec->{key},   $self->{config}->{$rec->{key}});
        local $~ = "FORMAT_VARS";
        write;
    }
    $self->telnet_prompt(lang('enter_starts'));

    $self->print(lang('now_unarchiving', $self->{product}), 'none');

# Do the upgrade
    $self->upgrade;
    $self->print(lang('upgrade_done', $self->{product}, $self->{version}), 'none');
    my $print;
    my $msg = $self->{upgrade_exit_message};
    if (ref $msg eq 'CODE') {
        $print = $msg->($self);
    }
    else {
        $print = $msg;
    }
    for (keys %{$self->{config}}) { $print =~ s/<%\Q$_\E%>/$self->{config}->{$_}/g }
    $self->print($print, 'none') if $print;
    $self->print("Gossamer Threads Inc.\n\n", 'none');
}

sub telnet_install {
# -------------------------------------------------------------------
# Performs a telnet install
#
    my ($self) = @_;

    local $_;
    PROMPTS: foreach my $rec (@{$self->{prompts}}) {
        if ($rec->{type} eq 'message') {
            $self->print($rec->{message}, $rec->{format});
        }
        elsif ($rec->{type} eq 'create_dirs') {
            $self->{config}->{create_dirs} = ($self->telnet_prompt(lang('is_upgrade'), "No", ['^Y(?:es)?$', '^No?$']) =~ /^y/);
        }
        else {
            $self->load_cache;
            $self->telnet_prompt($rec->{message}, $self->{defaults}->{$rec->{key}}, $rec);
            $self->save_cache;
        }
        if ($rec->{telnet_callback} and ref($rec->{telnet_callback}) eq 'CODE') {
            $rec->{telnet_callback}->($self) or redo PROMPTS;
        }
    }
    $self->print(lang('we_have_it'), 'none');
    foreach my $rec (@{$self->{prompts}}) {
        next if $rec->{type} eq 'message';
        next if not defined $self->{config}->{$rec->{key}} or not length $self->{config}->{$rec->{key}};
        ($a, $b) = ($rec->{key},   $self->{config}->{$rec->{key}});
        local $~ = "FORMAT_VARS";
        write;
    }
    $self->telnet_prompt(lang('enter_starts'));
    $self->print(lang('now_unarchiving', $self->{product}), 'none');

# Do the install
    $self->install;
    $self->print(lang('install_done', $self->{product}), 'none');
    my $print;
    my $msg = $self->{install_exit_message};
    if (ref $msg eq 'CODE') {
        $print = $msg->($self);
    }
    else {
        $print = $msg;
    }
    for (keys %{$self->{config}}) { $print =~ s/<%\Q$_\E%>/$self->{config}->{$_}/g }
    $self->print($print, 'none') if defined $print;
    $self->print("\nGossamer Threads Inc.\n\n", 'none');
}

sub telnet_prompt {
# -------------------------------------------------------------------
# Prompts the user.
#
    my ($self, $question, $default, $rec) = @_;
    $question =~ s/<[^>]+>//g;
    my ($response, $out);

    while (1) {
        if (defined($default)) {
            $out = "$question [$default]: ";
        }
        else {
            $out = "$question: ";
        }
        if ($self->{use_defaults} and defined($default)) {
            $self->print($out);
            $response = $default;
        }
        else {
            $response = $self->getline($out);
        }
        $response =~ s/\r?\n//;
        $response =~ s/^\s*//;
        $response =~ s/\s*$//;
        if ($default and ($response =~ /^\s*$/)) {
            $self->{term} and $self->{term}->addhistory($default);
            $response = $default;
        }
        if ($response eq 'exit' or $response eq 'quit' or $response eq '\q') {
            exit;
        }

        if (!defined($default) and !defined($response)) {
            print "\n$question: ";
            next;
        }
        if ($rec and ref($rec) eq 'HASH') {
            $self->{config}->{$rec->{key}} = $response;
            if (!$self->format_validate($rec->{key})) {
                $self->print("\n$GT::Installer::error\n", 'none');
                next;
            }
            else {
                last;
            }
        }
        elsif ($rec and ref($rec) eq 'ARRAY') {
            my $match;
            for (@{$rec}) {
                if ($response =~ /$_/i) {
                    $match = 1;
                    last;
                }
            }
            if (!$match) {
                $self->print(lang('invalid_responce', $response), 'none');
                next;
            }
        }
        last;
    }
    return $response;
}

sub telnet_error {
    my ($self, $msg) = @_;
    $self->print(lang('telnet_err', $msg), 'none');
}

sub print {
# -------------------------------------------------------------------
# print wrapper for telnet install.
#
    my ($self, $msg, $format) = @_;
    $format ||= 'none';
    if ($self->{is_cgi})   { return print $msg; }
    $msg =~ s/<[^>]+>//g;
    if ($format eq 'none') { return print $msg; }
    $msg = $self->linewrap(62, \$msg);
    local $_ = $msg;
    local $~ = $format;
    return write;
}

sub out {
# -------------------------------------------------------------------
# See what our output filehandle is if we are ran from telnet. It
# seems that some term functions need to have output sent to a 
# specific handle.
#
    my ($self) = @_;
    return $self->{term} ? ($self->{term}->OUT || *STDOUT) : *STDOUT;
}

sub getline {
# -------------------------------------------------------------------
# Getline function for telnet installes.
#
    my ($self, $msg) = @_;
    my $ret;
    if ($self->{term}) {
        $ret = $self->{term}->readline($msg || '');
    }
    else {
        $ret = <STDIN>;
    }
    return $ret;
}

sub save_cache {
# -------------------------------------------------------------------
# Saves the config cache for telnet installs.
#
    my ($self) = @_;
    return 1 unless ($self->{save_cache});

    my $defaults = do 'config.cache';
    $defaults ||= {};
    foreach my $key (keys %{$self->{config}}) {
        $defaults->{$key} = $self->{config}->{$key} || $self->{defaults}->{$key} || undef;
    }
    my $data = GT::Dumper->dump(data => $defaults, var => '');
    open CFG, ">config.cache" or return;
    print CFG $data;
    close CFG;

    return 1;
}

sub load_cache {
# -------------------------------------------------------------------
# Loads the config cache for telnet installes.
#
    my ($self) = @_;
    return 1 unless ($self->{save_cache});

    my $defaults = do 'config.cache';
    return unless $defaults;
    for my $key (keys %{$defaults}) {
        $self->{defaults}->{$key} = $defaults->{$key};
    }
    return 1;
}

#################################################################
#                             Formats                           #
#################################################################
# -------------------------------------------------------------------
# Format used to print final output of all vars right before unter
# happens in telnet.
#
format FORMAT_VARS = 
  @>>>>>>>>>>>>>>>>>>>>>>>>> : @<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<
                         $a,   $b
.

# -------------------------------------------------------------------
# Pretty scroll to put messages in in telnet.
#
format scroll1 =
 _________________________________________________________________
/\                                                                \
\_|                                                                |
  | @<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<< |~~
    munch($_)
  |  ______________________________________________________________|_
  \_/_______________________________________________________________/

.

# -------------------------------------------------------------------
# Pretty scroll to put messages in in telnet.
#
format scroll2 =
@<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<
" __^__                                                                __^__"
@<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<
"( ___ )--------------------------------------------------------------( ___ )"
 | / | @<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<< | \ |~~
       munch($_)
 |___|                                                                |___|
(_____)--------------------------------------------------------------(_____)

.

# -------------------------------------------------------------------
# More professional format for message output in telnet.
#
format professional =
@<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<
"#================================================================#"
| @<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<< |~~
   munch($_)
@<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<
"#================================================================#"
.

# -------------------------------------------------------------------
# Empty format mainly to wrape text properly
#
format plain =

@<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<~~
munch($_)

.

# -------------------------------------------------------------------
# Empty format mainly to wrape text properly
#
format plain_nolines =
@<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<~~
munch($_)
.

sub munch {
# -------------------------------------------------------------------
# Used to eat lines for proper formatting in the format fields 
# above.
#
    return '' unless length($_[0]) and defined($_[0]);
    my $idx = index($_[0], "\n");
    if ($idx == -1) {
        my $cp = $_[0];
        $_[0] = '';
        return $cp;
    }
    elsif ($idx == 0) {
        substr($_[0], 0, 1) = '';
        return ' ';
    }
    my $ret = substr($_[0], 0, $idx + 1);
    substr($_[0], 0, $idx + 1) = '';
    return $ret;
}

#################################################################
#                   Upgrade/Install HTML Prompts                #
#################################################################
sub cgi_first_screen {
# -----------------------------------------------------------------------------
# Initial screen displayed after EULA acceptance for cgi install/upgrade
#
    my ($self, $msg) = @_;
    $msg ||= '';
    my $path = $self->{in}->param('install_dir') || '';
    $self->parse_print(first_screen => {
        path    => $path,
        error   => $msg,
        message => @{$self->{initial_message}} ? $self->{initial_message}->[0] : '',
    });
}

sub parse_print {
    my ($self, $tag, $vars) = @_;
    my %vars = (
        %$vars,
        in      => $self->{in},
        product => $self->{product},
        version => $self->{version},
        lite    => $self->{lite},
        config  => $self->{config},
        error_breakout => $Error_Breakout
    );
    $self->{parsed_css} ||= GT::Template->parse('css', \%vars, { string => $GT::Installer::LANG{INSTALLER_CSS}, escape => 1, strict => 1});
    $vars{css} = $self->{parsed_css};
    GT::Template->parse_print($tag, \%vars, { string => $GT::Installer::LANG{uc $tag}, escape => 1, strict => 1 });
}

sub cgi_upgrade_first_screen {
# -------------------------------------------------------------------
# Initial screen for cgi upgrade
#
    my ($self, $msg) = @_;
    $msg ||= '';

# Make the form based on self->{upgrade}
    my $upgrade_form = '';
    my @rows = @{$self->{upgrade}};
    my $i = 0;

# Check for any questions that still need to be asked as load_config couldn't handle them
    my @fields = @{$self->{prompts}};
    my @questions;
    my $last_message;
    for (@fields) {
        if ($_->{type} eq 'message') {
            $last_message = $_;
            next;
        }
        if ($self->{need_upgrade_answer}->{$_->{key}}) {
            push @questions, $last_message if $last_message;
            push @questions, $_;
            $_->{value} = $self->{in}->param($_->{key}) || $self->{config}->{$_->{key}} || $self->{defaults}->{$_->{key}};
        }
        $last_message = undef;
    }
    $self->parse_print(upgrade_first_screen => {
        error         => $msg,
        upgrade_form  => $upgrade_form,
        GT_ADMIN_PATH => $self->{config}->{GT_ADMIN_PATH},
        rows          => \@rows,
        fields        => \@questions
    });
}

sub cgi_upgrade_second_screen {
# -------------------------------------------------------------------
# Third screen for cgi upgrade.
# Sets the user overwrite/skip/backup options and performs the 
# upgrade.
#
    my ($self) = @_;

    my @errors;
    for my $rec (@{$self->{prompts}}) {
        next if $rec->{type} and $rec->{type} eq 'message';
        next unless $self->{need_upgrade_answer}->{$rec->{key}};
        $self->{config}->{$rec->{key}} = $self->{in}->param($rec->{key});
        if (!$self->format_validate($rec->{key})) {
            push @errors, $GT::Installer::error;
        }
    }
    @errors and return $self->cgi_upgrade_first_screen('<ul>' . join('', map "<li>$_</li>", @errors) . '</ul>');

    $self->parse_print(upgrade_second_screen_first => {});

    $Error_Breakout = '</pre></blockquote>';

# Do the upgrade, This method prints as it goes
    my $i = 0;
    foreach my $rec (@{$self->{upgrade}}) {
        next if $rec->{skip};
        next if ($rec->{type} and $rec->{type} eq 'message');
        if ($self->{in}->param($i)) {
            $rec->{answer} = $self->{in}->param($i);
        }
        else {
            $rec->{answer} = 'b';
        }
        $i++;
    }

    # Turn on buffering as we are about to produce a whole lot of output, and
    # buffering it slows everything down significantly.
    $| = 0;

    $self->upgrade;
    my $msg;
    if (-e 'install.cgi') {
        $msg = lang('install_warning');
    }
    else {
        $msg = lang('install_removed');
    }

# Anything left in the upgrade array should be a message
    my $msg2 = $self->{upgrade_exit_message};
    my $print;
    if (ref $msg2 eq 'CODE') {
        $print = $msg2->($self);
    }
    else {
        $print = $msg2;
    }

    my $regex = '(' . join('|', map quotemeta, keys %{$self->{config}}) . ')';
    $print =~ s/<%$regex%>/$self->{config}->{$1}/g;

    $self->parse_print(upgrade_second_screen_second => {
        install_message => $msg,
        message         => $print
    });

    print "<!-- GTINST: done -->";
    $REMOVE_INSTALL = 1;
}

sub cgi_install_first_screen {
# -------------------------------------------------------------------
# Initial screen for cgi install
# Prompts for programmer defined config options.
#
    my ($self, $msg) = @_;
    $msg ||= '';
    #my $msg_formatted = $msg ? qq~<p><font face="Tahoma,Arial,Helvetica" size="2" color="red">$msg</font>~ : '';

    my @fields = @{$self->{prompts}};
    for (@fields) {
        $_->{value} = $self->{in}->param($_->{key}) || $self->{config}->{$_->{key}} || $self->{defaults}->{$_->{key}};
    }
    $self->parse_print(install_first_screen => {
        error  => $msg,
        fields => \@fields
    });

    if ($msg) {
        my $raw = $msg;
        $raw =~ s,^\s*<li>,,;
        $raw =~ s/<li>/,/g;
        $raw =~ s/<.*?>//g;
        print "<!-- GTERR: [$raw] -->";
    }
}

sub cgi_install_second_screen {
# -------------------------------------------------------------------
# Second screen for cgi install.
# Validates form input and performs the install.
#
    my ($self) = @_;

    my @errors;
    foreach my $rec (@{$self->{prompts}}) {
        next if ($rec->{type} and $rec->{type} eq 'message');
        if ($rec->{type} eq 'create_dirs') {
            $self->{config}->{create_dirs} = defined $self->{in}->param('create_dirs');
            next;
        }
        $self->{config}->{$rec->{key}} = $self->{in}->param($rec->{key});
        if (!$self->format_validate($rec->{key})) {
            push @errors, $GT::Installer::error;
        }
    }
    @errors and return $self->cgi_install_first_screen('<ul>' . join('', map "<li>$_</li>", @errors) . '</ul>');

# Print the header.
    $self->parse_print(install_second_screen_first => {});

    $Error_Breakout = '</pre></blockquote>';

    # Turn on buffering as we are about to produce a whole lot of output, and
    # buffering it slows everything down significantly.
    $| = 0;

# Do the upgrade, This method prints as it goes
    $self->install;

    my $msg;
    if (-e 'install.cgi') {
        $msg = lang('install_warning');
    }
    else {
        $msg = lang('install_removed');
    }
    my $msg2 = $self->{install_exit_message};
    my $print;
    if (ref $msg eq 'CODE') {
        $print = $msg2->($self);
    }
    else {
        $print = $msg2;
    }

    my $regex = '(' . join('|', map quotemeta, keys %{$self->{config}}) . ')';
    $print =~ s/<%$regex%>/$self->{config}->{$1}/g;

    $self->parse_print(install_second_screen_second => {
        install_message => $msg,
        message         => $print
    });
    print "<!-- GTINST: done -->";

    $REMOVE_INSTALL = 1;
}

sub cgi_error {
    my ($self, $msg) = @_;
    $msg ||= '';

    $self->parse_print(cgi_error_screen => { error => $msg });

    if ($msg) {
        my $raw = $msg;
        $raw =~ s,^\s*<li>,,;
        $raw =~ s/<li>/,/g;
        $raw =~ s/<.*?>//g;
        print "<!-- GTERR: [$raw] -->";
    }
}

#################################################################
#                        Utility Functions                      #
#################################################################
sub find_perl {
# ---------------------------------------------------------------
# Returns path to perl.
#
    my @poss_perls = qw!
         /usr/local/bin/perl /usr/bin/perl /bin/perl /perl/bin/perl.exe
         /usr/local/bin/perl5 /usr/bin/perl5 /bin/perl5
         c:/perl/bin/perl.exe d:/perl/bin/perl.exe
    !;

    for my $perl_path (@poss_perls) {
        return $perl_path if -x $perl_path and -f _;
    }
    return '';
}

sub find_sendmail {
# ---------------------------------------------------------------
# Try and figure out where sendmail is located.
#
    for (qw|/usr/sbin/sendmail /usr/lib/sendmail /bin/sendmail|) {
        return $_ if -f and -x _;
    }
    return '';
}

sub find_cgi {
# ---------------------------------------------------------------
# Try and figure out the program path.
#
    if (!GT::Base::PERSIST) {
        # FindBin *does not work* under persistent environment.
        eval {
            local $SIG{__DIE__};
            local $SIG{__WARN__} = sub {}; # Silence messages from Carp
            require FindBin;
        };

        if (defined $FindBin::Bin and length $FindBin::Bin) {
            my $found = $FindBin::Bin;
            $found =~ s|/$||;
            return $found;
        }
    }

    return $1 if $0 =~ m{(.*)/[^/]+$};
    return;
}

sub recurse_mkdir {
# ---------------------------------------------------------------
# Makes a directoy recursivly.
#
    my ($self, $dir) = @_;
    my @path = split m|/|, $dir;
    pop @path unless substr($dir, -1) eq '/';
    my @subpath; # /foo/bar/baz/ -> ('/foo/bar/baz', '/foo/bar', '/foo', '')
    for (reverse 0 .. $#path) {
        push @subpath, join '/', @path[0 .. $_], '';
    }
    for my $i (0 .. $#subpath) {
        my $path = $subpath[$i];
        next if $path eq '';

        if (-e $path and not -d _) { return $self->error('DIREXISTS', 'WARN', $path) }
        elsif (-d _) {
            for (reverse 0 .. $i-1) {
                mkdir $subpath[$_], 0777 or return $self->error('MKDIR', 'WARN', $subpath[$_], "$!");
                chmod 0755, $subpath[$_];
            }
            last;
        }
    }
    return 1;
}


sub merge_hash {
# -----------------------------------------------------------
# $class->merge_hash($hash1, $hash2);
# ----------------------------------------------
# Merges two data structures together. The first argument is the data structure
# to merge into. $data1 is changed based on $data2. Nothing is returned. Only
# hashes are tested.
#
    my ($self, $to, $from)  = @_;

    for (keys %$from) {
# If the value is a hash and it exists in the structure we are comparing it with recurse.
        if (ref $from->{$_} eq 'HASH') {
            if (exists $to->{$_} and ref $to->{$_} eq 'HASH') {
                $self->merge_hash($to->{$_}, $from->{$_});
            }
            else {
                $to->{$_} = $from->{$_}; # If $from contains a hash, and $to contains a non-hash, not updating may break the code.
            }
        }
        elsif (not exists $to->{$_}) {
            $to->{$_} = $from->{$_};
        }
    }
    return 1;
}

sub linewrap {
# --------------------------------------------------------------------
# $self->linewrap($length, \$string);
# -----------------------------------
#   Takes $string and wraps it on length. This method tries to wrap on
#   white spaces. If there can be no white space found before the 
#   beginning of the line it wraps it on exactly $length. 
#   URL's are not wrapped.
#
    my ($self, $i, $string) = @_;
    my @t = split /\n/, $$string;
    my $r = ' ' x length $$string; $r = '';

    while (@t) {
        my $length = length $t[0];
        if ($length <= $i or $t[0] =~ /^\s*$/) {
            $r .= shift(@t) . "\n";
        }
        elsif ($length > 50000) { # Line is too long.
            my $line = shift @t;
            while ($line) {
                $r .= substr($line, 0, $i) . "\n";
                substr($line, 0, $i) = '';
            }
        }
        else {
            $r .= _unexpand($i, (shift(@t) || ''));
        }
    }
    return $r;
}
sub _unexpand {
# --------------------------------------------------------------------
# _unexpand($length, $string);
# ----------------------------
#   Internal method called by linewrap() to wrape a line.
#
    my ($i, $e);
    $i = $e = shift;
    my $r;
    while (@_) {
        defined($_[0]) or last;
        if ($_[0] =~ /^(.{$i})\s(.+)$/) {
            shift() and $r .= $1 . "\n";
            $i = $e;
            if (defined($2) and length($2) <= $e) { $r .= $2 ."\n" }
            else { unshift(@_, $2) }
        }
        elsif ($i-- == 0) {
            $i = $e;
            shift() =~ /^(.{$i})(.+)$/ and $r .= $1 . "\n";
            if (defined($2) and length($2) <= $e) { $r .= $2 }
            else { unshift(@_, $2) }
        }
    }
    return defined($r) ? $r : '';
}

sub tpllang {
    return \lang(@_);
}

sub lang {
    my $lang = uc shift;
    return exists $GT::Installer::LANG{$lang} ? sprintf($GT::Installer::LANG{$lang}, @_) : $lang;
}

1;


} # End of BEGIN for GT/Installer.pm

BEGIN {
    $INC{"GT/File/Tools.pm"} = "GT/File/Tools.pm";

# ==================================================================
# Gossamer Threads Module Library - http://gossamer-threads.com/
#
#   GT::File::Tools
#   Author : Scott Beck
#   $Id: Tools.pm,v 1.64 2007/02/10 17:45:41 sbeck Exp $
#
# Copyright (c) 2004 Gossamer Threads Inc.  All Rights Reserved.
# ==================================================================
#
# Description: Basic file tools
#

package GT::File::Tools;
# ==================================================================

use strict;
use vars qw/
    $VERSION
    @EXPORT_OK
    %EXPORT_TAGS
    $MAX_DEPTH
    $GLOBBING
    $ERRORS
    $MAX_READ
    $DEBUG
    $NO_CHDIR
    $REGEX
    $UNTAINT
    $error
/;
$REGEX = '^([^\0]+)$';

use bases 'GT::Base' => '';

use Cwd;
require Exporter;
use GT::AutoLoader;
$VERSION = sprintf "%d.%03d", q$Revision: 1.64 $ =~ /(\d+)\.(\d+)/;

# Exporter variables
@EXPORT_OK = qw/
    copy
    move
    del
    deldir
    find
    mkpath rmkdir
    parsefile
    basename
    filename
    dirname
    expand 
/;
%EXPORT_TAGS = ( all => \@EXPORT_OK );
*import = \&Exporter::import;

# Options
$MAX_DEPTH = 1000;
$GLOBBING = 0;
$NO_CHDIR = 0;
$MAX_READ = 1024 * 64;
$UNTAINT  = 0;
$DEBUG = 0;
$ERRORS = {
    UNLINK    => "Could not unlink '%s': %s",
    RMDIR     => "Could not rmdir '%s': %s",
    MOVE      => "Could not move '%s' to '%s': %s",
    RENAME    => "Could not rename '%s' to '%s': %s",
    SYMLINK   => "Could not symlink '%s' to '%s': %s",
    NOTAFILE  => "File to copy, move, or del ('%s') is not a regular file",
    NOTADIR   => "Path passed to find ('%s') is not a directory",
    TOODEEP   => "Recursive find surpassed max depth. Last path was %s",
    RECURSIVE => "Circular symlinks detected",
    OPENDIR   => "Could not open directory '%s': %s",
    READOPEN  => "Could not open '%s' for reading: %s",
    WRITEOPEN => "Could not open '%s' for writing: %s"
};

$COMPILE{move} = __LINE__ . <<'END_OF_SUB';
sub move {
# ----------------------------------------------------------------------------
    my $class = 'GT::File::Tools';

    $class->fatal( BADARGS => "No arguments passed to move()" )
        unless @_;

    my $opts = ref $_[-1] eq 'HASH' ? pop : {};

    my $to = pop;
    $class->fatal( BADARGS => "No place to move files to specified for move()" )
        unless defined $to;

    my $globbing = delete $opts->{globbing};
    $globbing = $GLOBBING unless defined $globbing;

    my $untaint = delete $opts->{untaint};
    $untaint = $UNTAINT unless defined $untaint;

    my $untaint_regex = delete $opts->{untaint_regex};
    $untaint_regex = $REGEX unless defined $untaint_regex;

    my @files = @_;
    @files = expand( @files ) if $globbing;

    $class->fatal( BADARGS => "No files to move" )
        unless @files;

    my $error_handler = delete $opts->{error_handler};
    $error_handler = sub { $class->warn( @_ ); 1 }
        unless defined $error_handler;

    $class->fatal(
        BADARGS => "error_handler option must be a code reference"
    ) unless ref $error_handler eq 'CODE';

    my $max_depth = delete $opts->{max_depth};
    $max_depth = $MAX_DEPTH unless defined $max_depth;

    $class->fatal(
        BADARGS => "Unknown option " . ( join ", ", keys %$opts )
    ) if keys %$opts;

    my %seen;
    for my $from_file ( @files ) {
        my $to_file = $to;
        if ( !-d $to and $seen{$to}++ ) {
            $class->fatal(
                BADARGS => "Trying to move multiple files into one file"
            );
        }
        if ( -d $from_file ) {
            $class->debug( "movedir $from_file, $to_file" ) if $DEBUG > 1;
            movedir(
                $from_file, $to_file,
                {
                    error_handler   => $error_handler,
                    max_depth       => $max_depth,
                    untaint         => $untaint,
                    untaint_regex   => $untaint_regex,
                }
            ) or return;
            next;
        }
        if ( -d $to_file ) {
            $to_file = $to . '/' . basename( $from_file );
        }
        if ($untaint) {
            $to_file =~ $untaint_regex and $to_file = $1;
            is_tainted($to_file) and die "bad file $to_file";
            $from_file =~ $untaint_regex and $from_file = $1;
            is_tainted($from_file) and die "bad file $from_file";
        }
        if ( -l $from_file ) {
            my ( $link ) = _fix_symlink( $from_file );
            if ( !symlink $link, $to_file ) {
                $error_handler->( SYMLINK => $from_file, $to_file, "$!" )
                    or return;
            }
            if ( !unlink $from_file ) {
                $error_handler->( UNLINK => $from_file, "$!" )
                    or return;
            }
            next;
        }
        my ( $to_size_before, $to_mtime_before ) = ( stat( $to_file ) )[7, 9];
        my $from_size = -s $from_file;
        $class->debug( "rename $from_file, $to_file" ) if $DEBUG > 1;
        next if rename $from_file, $to_file;
        my $err = "$!";
        my $errno = 0+$!;

# Under NFS rename can work but still return an error, check for that
        my ( $to_size_after, $to_mtime_after ) = ( stat( $to_file ) )[7, 9];
        if ( defined $from_size and -e $from_file ) {
            if (
                defined $to_mtime_before and
                ( 
                    $to_size_before != $to_size_after or
                    $to_mtime_before != $to_mtime_after
                ) and
                $to_size_after == $from_size
            )
            {
                $class->debug( "rename over NFS worked" ) if $DEBUG > 1;
                next;
            }
        }

        $class->debug( "copy $from_file, $to_file" ) if $DEBUG > 1;
        next if copy( $from_file, $to_file,
            {
                preserve_all    => 1,
                max_depth       => $max_depth,
                error_handler   => $error_handler,
                untaint         => $untaint,
                untaint_regex   => $untaint_regex,
            }
        ) and unlink $from_file;

# Remove if a particial copy happened
        if (
            !defined( $to_mtime_before )        or
            $to_mtime_before != $to_mtime_after or
            $to_size_before != $to_size_after
        )
        {
            unlink $to_file;
        }
        $error_handler->( RENAME => $from_file, $to_file, $err, $errno )
            or return;
    }
    return 1;
}
END_OF_SUB

$COMPILE{movedir} = __LINE__ . <<'END_OF_SUB';
sub movedir {
# ----------------------------------------------------------------------------
    my ( $from, $to, $opts ) = @_;
    my $class = 'GT::File::Tools';

    my $error_handler = delete $opts->{error_handler};
    $error_handler = sub { $class->warn( @_ ); 1 }
        unless defined $error_handler;

    $class->fatal(
        BADARGS => "error_handler option must be a code reference"
    ) unless ref $error_handler eq 'CODE';

    my $max_depth = delete $opts->{max_depth};
    $max_depth = $MAX_DEPTH unless defined $max_depth;

    my $untaint = delete $opts->{untaint};
    $untaint = $UNTAINT unless defined $untaint;

    my $untaint_regex = delete $opts->{untaint_regex};
    $untaint_regex = $REGEX unless defined $untaint_regex;

    $class->fatal(
        BADARGS => "Unknown option " . ( join ", ", keys %$opts )
    ) if keys %$opts;

    $from .= '/' unless $from =~ m,/\Z,;
    $to .= '/' unless $to =~ m,/\Z,;

# To move a directory inside an already existing directory
    $to .= basename( $from ) if -d $to;

# Try the easy way out first
    return 1 if rename $from, $to;

    my $cwd;
    if ( ( parsefile( $from ) )[2] ) {
        $cwd = mycwd();
        $from = "$cwd/$from";
    }
    if ( ( parsefile( $to ) )[2] ) {
        $cwd ||= mycwd();
        $to = "$cwd/$to";
    }
    if ($untaint) {
        $to =~ $untaint_regex and $to = $1;
        is_tainted($to) and die "bad file $to";
        $from =~ $untaint_regex and $from = $1;
        is_tainted($from) and die "bad file $from";
    }

    return find(
        $from,
        sub {
            my ( $path ) = @_;
            if ( -l $path ) {
                $path .= '/' if ( -d _ and $path !~ m,/\Z, );
                my ( $link, $relative ) = _fix_symlink( $path );
                ( my $new_path = $path ) =~ s!\A\Q$from!$to!;
                $class->debug( "link $link, $new_path" ) if $DEBUG > 1;
                unless (-l $new_path) {
                    symlink $link, $new_path
                        or $error_handler->( SYMLINK =>  $link, $new_path, "$!" )
                        or return;
                }
                _preserve( $path, $new_path,
                    set_owner => 1,
                    set_time  => 1
                );
                unlink $path
                    or $error_handler->( UNLINK =>  $path, "$!" )
                    or return;
                return 1;
            }
            elsif ( -d $path ) {
                $path .= '/' unless $path =~ m,/\Z,;
                ( my $new_path = $path ) =~ s!\A\Q$from!$to!;
                $class->debug( "mkdir $new_path" ) if $DEBUG > 1;
                unless (-d $new_path) {
                    mkdir $new_path, 0777
                        or $error_handler->( MKDIR =>  $new_path, "$!" )
                        or return;
                }
                _preserve( $path, $new_path,
                    set_perms => 1,
                    set_owner => 1,
                    set_time  => 1
                );
                rmdir $path
                    or $error_handler->( RMDIR => $path, "$!" )
                    or return;
            }
            elsif ( -f _ ) {
                ( my $new_path = $path ) =~ s!\A\Q$from!$to!;
                $class->debug( "move $path, $new_path" ) if $DEBUG > 1;
                move( $path, $new_path,
                    {
                        error_handler   => $error_handler,
                        max_depth       => $max_depth,
                    }
                )   or $error_handler->( MOVE => $path, $new_path, "$!" )
                    or return;
            }
            else {
                $error_handler->( NOTAFILE => $path ) or return;
            }
            return 1;
        },
        {
            dirs_first      => 1,
            error_handler   => $error_handler,
            max_depth       => $max_depth,
            untaint         => $untaint,
            untaint_regex   => $untaint_regex,
        }
    );
}
END_OF_SUB

$COMPILE{del} = __LINE__ . <<'END_OF_SUB';
sub del {
# ----------------------------------------------------------------------------
    my $class = 'GT::File::Tools';
    my $opts = ref $_[-1] eq 'HASH' ? pop : {};

    my $error_handler = delete $opts->{error_handler};
    $error_handler = sub { $class->warn( @_ ); 1 } unless $error_handler;

    $class->fatal(
        BADARGS => "error_handler option must be a code reference"
    ) unless ref $error_handler eq 'CODE';

    my $globbing = delete $opts->{globbing};
    $globbing = $GLOBBING unless defined $globbing;

    my @files = @_;
    @files = expand( @files ) if $globbing;

    $class->fatal( BADARGS => "No directories to delete" )
        unless @files;

    my $untaint = delete $opts->{untaint};
    $untaint = $UNTAINT unless defined $untaint;

    my $untaint_regex = delete $opts->{untaint_regex};
    $untaint_regex = $REGEX unless defined $untaint_regex;

    $class->fatal(
        BADARGS => "Unknown option " . ( join ", ", keys %$opts )
    ) if keys %$opts;

    for my $path ( @files ) {
        if ($untaint) {
            $path =~ $untaint_regex and $path = $1;
            is_tainted($path) and die "bad file $path";
        }
        if ( -l $path ) {
            $class->debug( "unlink $path" ) if $DEBUG > 1;
            unlink $path
                or $error_handler->( UNLINK => $path, "$!" )
                or return;
        }
        elsif ( -d $path ) {
            $error_handler->( NOTAFILE => $path )
                or return;
        }
        else {
            unlink $path
                or $error_handler->( UNLINK => $path, "$!" )
                or return;
        }
    }
    return 1;
}
END_OF_SUB

$COMPILE{deldir} = __LINE__ . <<'END_OF_SUB';
sub deldir {
# ----------------------------------------------------------------------------
    my $class = 'GT::File::Tools';
    my $opts = ref $_[-1] eq 'HASH' ? pop : {};

    my $error_handler = delete $opts->{error_handler};
    $error_handler = sub { $class->warn( @_ ); 1 } unless $error_handler;

    $class->fatal(
        BADARGS => "error_handler option must be a code reference"
    ) unless ref $error_handler eq 'CODE';

    my $globbing = delete $opts->{globbing};
    $globbing = $GLOBBING unless defined $globbing;

    my @dirs = @_;
    @dirs = expand( @dirs ) if $globbing;

    $class->fatal( BADARGS => "No directories to delete" )
        unless @dirs;

    my $max_depth = delete $opts->{max_depth};
    $max_depth = $MAX_DEPTH unless defined $max_depth;

    my $untaint = delete $opts->{untaint};
    $untaint = $UNTAINT unless defined $untaint;

    my $untaint_regex = delete $opts->{untaint_regex};
    $untaint_regex = $REGEX unless defined $untaint_regex;

    $class->fatal(
        BADARGS => "Unknown option " . ( join ", ", keys %$opts )
    ) if keys %$opts;

    for my $dir ( @dirs ) {
        if ($untaint) {
            $dir =~ $untaint_regex and $dir = $1;
            is_tainted($dir) and die "bad file $dir";
        }
        next unless -e $dir or -l $dir;

# Try the easy way out first
        next if rmdir $dir or unlink $dir;

        find(
            $dir,
            sub {
                my ( $path ) = @_;
                if ( -l $path ) {
                    $class->debug( "unlink $path" ) if $DEBUG > 1;
                    unlink $path
                        or $error_handler->( UNLINK => $path, "$!" )
                        or return;
                }
                elsif ( -d $path ) {
                    $class->debug( "rmdir $path" ) if $DEBUG > 1;
                    rmdir $path
                        or $error_handler->( RMDIR => $path, "$!" )
                        or return;
                }
                else {
                    $class->debug( "unlink $path" ) if $DEBUG > 1;
                    unlink $path
                        or $error_handler->( UNLINK => $path, "$!" )
                        or return;
                }
                return 1;
            },
            {
                dirs_first      => 0,
                error_handler   => $error_handler,
                max_depth       => $max_depth,
                untaint         => $untaint,
                untaint_regex   => $untaint_regex,
            }
        );
    }
    return 1;
}
END_OF_SUB

$COMPILE{copy} = __LINE__ . <<'END_OF_SUB';
sub copy {
# ----------------------------------------------------------------------------
    my $class = 'GT::File::Tools';

    $class->fatal( BADARGS => "No arguments passed to move()" )
        unless @_;

    my $opts = ref $_[-1] eq 'HASH' ? pop : {};
    my $to = pop;
    $class->fatal( BADARGS => "No place to move files to specified for move()" )
        unless defined $to;

    my $globbing = delete $opts->{globbing};
    $globbing = $GLOBBING unless defined $globbing;

    my $untaint = delete $opts->{untaint};
    $untaint = $UNTAINT unless defined $untaint;

    my $untaint_regex = delete $opts->{untaint_regex};
    $untaint_regex = $REGEX unless defined $untaint_regex;

    my @files = @_;
    @files = expand( @files ) if $globbing;

    $class->fatal( BADARGS => "No files to move" )
        unless @files;

    my $error_handler = delete $opts->{error_handler};
    $error_handler = sub { $class->warn( @_ ); 1 }
        unless defined $error_handler;

    $class->fatal(
        BADARGS => "error_handler option must be a code reference"
    ) unless ref $error_handler eq 'CODE';

    my %preserve_opts = (set_perms => 1);
    if ( delete $opts->{preserve_all} ) {
        @preserve_opts{qw/set_perms set_owner set_time/} = ( 1, 1 ,1 );
    }
    else {
        $preserve_opts{set_perms} = delete $opts->{set_perms} if defined $opts->{set_perms};
        @preserve_opts{qw/set_owner set_time/} =
        (
            delete $opts->{set_owner},
            delete $opts->{set_time}
        );
    }

    my $max_depth = delete $opts->{max_depth};
    $max_depth = $MAX_DEPTH unless defined $max_depth;

    $class->fatal(
        BADARGS => "Unknown option " . ( join ", ", keys %$opts )
    ) if keys %$opts;

    my %seen;
    for my $from_file ( @files ) {
        my $to_file = $to;
        if ( !-d $to_file and $seen{$to_file}++ ) {
            $class->fatal(
                BADARGS => "Trying to copy multiple files into one file $from_file => $to"
            );
        }
        if ( -d $from_file ) {
            $class->debug( "copydir $from_file, $to_file" ) if $DEBUG > 1;
            copydir( $from_file, $to_file, {
                error_handler   => $error_handler,
                max_depth       => $max_depth,
                untaint         => $untaint,
                untaint_regex   => $untaint_regex,
                %preserve_opts
            });
            next;
        }
        if ( -d $to_file ) {
            $to_file = $to . '/' . basename( $from_file );
        }
        if ($untaint) {
            $to_file =~ $untaint_regex and $to_file = $1;
            is_tainted($to_file) and die "bad file $to_file";

            $from_file =~ $untaint_regex and $from_file = $1;
            is_tainted($from_file) and die "bad file $from_file";
        }

        if ( -l $from_file ) {
            my ( $link ) = _fix_symlink( $from_file );
            if ($untaint) {
                $link =~ $untaint_regex and $link = $1;
                is_tainted($link) and die "bad file $link";
            }

            if ( !symlink $link, $to_file ) {
                $error_handler->( SYMLINK => $from_file, $to_file, "$!" )
                    or return;
            }
            next;
        }

        local( *FROM, *TO );
        $class->debug( "open $from_file" ) if $DEBUG > 1;
        unless ( open FROM, "< $from_file" ) {
            $error_handler->( READOPEN => $from_file, "$!" ) or return;
            next;
        }
        $class->debug( "open $to_file" ) if $DEBUG > 1;
        unless ( open TO, "> $to_file" ) {
            $error_handler->( WRITEOPEN => $to_file, "$!" ) or return;
            next;
        }
        binmode FROM or $class->fatal( BINMODE => "$!" );
        binmode TO or $class->fatal( BINMODE => "$!" );
        my $size = -s FROM;
        $size = $MAX_READ if $size > $MAX_READ;

        while () {
            my ( $ret, $buf );
            $ret = sysread FROM, $buf, $size;
            $class->fatal( READ => "$!" )
                unless defined $ret;
            last unless $ret;
            $ret = syswrite TO, $buf, length $buf;
            $class->fatal( WRITE => "$!" )
                unless defined $ret;
        }

        close FROM;
        close TO;

# Set permissions, mtime, and owner
        _preserve( $from_file, $to_file, %preserve_opts );

    }
    return 1;
}
END_OF_SUB

$COMPILE{copydir} = __LINE__ . <<'END_OF_SUB';
sub copydir {
# ----------------------------------------------------------------------------
    my ( $from, $to, $opts ) = @_;
    my $class = 'GT::File::Tools';

    $class->fatal( BADARGS => "No from directory specified" )
        unless defined $from;
    $class->fatal( BADARGS => "From file specified must be a directory" )
        unless -d $from;
    $class->fatal( BADARGS => "No to directory specified" )
        unless defined $from;
    my $error_handler = delete $opts->{error_handler};

    $error_handler = sub { $class->warn( @_ ); 1 }
        unless defined $error_handler;

    $class->fatal(
        BADARGS => "error_handler option must be a code reference"
    ) unless ref $error_handler eq 'CODE';

    my %preserve_opts = (set_perms => 1);
    if ( delete $opts->{preserve_all} ) {
        @preserve_opts{qw/set_perms set_owner set_time/} = ( 1, 1 ,1 );
    }
    else {
        $preserve_opts{set_perms} = delete $opts->{set_perms} if defined $opts->{set_perms};
        @preserve_opts{qw/set_owner set_time/} =
        (
            delete $opts->{set_owner},
            delete $opts->{set_time}
        );
    }

    my $untaint = delete $opts->{untaint};
    $untaint = $UNTAINT unless defined $untaint;

    my $untaint_regex = delete $opts->{untaint_regex};
    $untaint_regex = $REGEX unless defined $untaint_regex;

    my $max_depth = delete $opts->{max_depth};
    $max_depth = $MAX_DEPTH unless defined $max_depth;

    $class->fatal(
        BADARGS => "Unknown option " . ( join ", ", keys %$opts )
    ) if keys %$opts;

    $from .= '/' unless $from =~ m,/\Z,;
    $to .= '/' unless $to =~ m,/\Z,;

# To move a directory inside an already existing directory
    $to .= basename( $from ) if -d $to;

    my $cwd;
    if ( ( parsefile( $from ) )[2] ) {
        $cwd = mycwd();
        if ($untaint) {
            $cwd =~ $untaint_regex and $cwd = $1;
            is_tainted($cwd) and die "bad file $cwd";
        }

        $from = "$cwd/$from";
    }
    if ( ( parsefile( $to ) )[2] ) {
        $cwd ||= mycwd();
        $to = "$cwd/$to";
    }
    if ($untaint) {
        $to =~ $untaint_regex and $to = $1;
        is_tainted($to) and die "bad file $to";
        $from =~ $untaint_regex and $from = $1;
        is_tainted($from) and die "bad file $from";
    }
    $from =~ s{/\Z}{};
    $to =~ s{/\Z}{};

    return find(
        $from,
        sub {
            my ( $path ) = @_;
            if ( -l $path ) {
                $path .= '/' if ( -d _ and $path !~ m,/\Z, );
                my ( $link, $relative ) = _fix_symlink( $path );
                ( my $new_path = $path ) =~ s!\A\Q$from!$to!;
                $class->debug( "link $link, $new_path" ) if $DEBUG > 1;
                unless (-l $new_path) {
                    if ($untaint) {
                        $link =~ $untaint_regex and $link = $1;
                        is_tainted($link) and die "bad file $link";
                    }

                    symlink $link, $new_path
                        or $error_handler->( SYMLINK =>  $link, $new_path, "$!" )
                        or return;
                }
                _preserve( $path, $new_path, %preserve_opts );
                return 1;
            }
            elsif ( -d $path ) {
                $path .= '/' unless $path =~ m,/\Z,;
                ( my $new_path = $path ) =~ s!\A\Q$from!$to!;
                $class->debug( "mkdir $new_path" ) if $DEBUG > 1;
                unless (-d $new_path) {
                    mkdir $new_path, 0777
                        or $error_handler->( MKDIR =>  $new_path, "$!" )
                        or return;
                }
                _preserve( $path, $new_path, %preserve_opts );
            }
            elsif ( -f $path ) {
                $from =~ s{/\Z}{};
                $to =~ s{/\Z}{};

                ( my $new_path = $path ) =~ s!\A\Q$from!$to!;
                $class->debug( "copy $path, $new_path" ) if $DEBUG > 1;
                copy( $path, $new_path,
                    {
                        %preserve_opts,
                        error_handler   => $error_handler,
                        max_depth       => $max_depth,
                        untaint         => $untaint,
                        untaint_regex   => $untaint_regex
                    }
                )
                    or $error_handler->( MOVE => $path, $new_path, "$GT::File::Tools::error" )
                    or return;
# copy() will handle setting permission and such
            }
            else {
                $error_handler->( NOTAFILE => $path )
                    or return;
            }
            return 1;
        }, 
        {
            dirs_first      => 1,
            error_handler   => $error_handler,
            max_depth       => $max_depth,
            untaint         => $untaint,
            untaint_regex   => $untaint_regex,
        }
    );
}
END_OF_SUB

$COMPILE{filename} = __LINE__ . <<'END_OF_SUB';
sub filename {
# -----------------------------------------------------------------------------
# Deprecated name for basename
#
    goto &basename;

}
END_OF_SUB

sub basename {
# -----------------------------------------------------------------------------
    return ( parsefile( $_[0] ) )[1];
}

sub dirname {
# ----------------------------------------------------------------------------
    return ( parsefile( $_[0] ) )[0];
}

$COMPILE{parsefile} = __LINE__ . <<'END_OF_SUB';
sub parsefile {
# ----------------------------------------------------------------------------
    my ( $in ) = @_;
    my ( @path, @normal, $relative, $win32 );
    if ( $^O eq 'MSWin32' ) {
        $win32 = $1 if $in =~ s/\A(\w:)//;
        @path = split m|[/\\]|, $in;
        $relative = 1 unless $in =~ m,\A[/\\],;
    }
    else {
        @path = split m|/|, $in;
        $relative = 1 unless $in =~ m,\A/,;
    }
    my $start = 0;
    for ( @path ) {
        if ( $_ eq '.' or !length ) { next }
        elsif ( $_ eq '..' ) { $start-- }
        else { $start++ }

        if ( !$relative and $start < 0 and $_ eq '..' ) { next }
        elsif ( $start < 0 and $_ eq '..' ) { push @normal, ".." }
        elsif ( $start >= 0 and $_ eq '..' ) { pop @normal }
        else { push @normal, $_ }
    }
    my $file = pop @normal;
    my $new_path = join "/", @normal;
    $new_path = $relative ? "./$new_path" : "/$new_path";
    $new_path = "$win32$new_path" if $win32;
    if ($new_path =~ /$REGEX/) {
        $new_path = $1 ;
    }
    else {
        die "Bad path $new_path";
    }
    if (length $file) {
        if ($file =~ /$REGEX/) {
            $file = $1 ;
        }
        else {
            die "Bad path $file";
        }
    }

    return ( $new_path, $file, $relative );
}
END_OF_SUB


$COMPILE{mkpath} = __LINE__ . <<'END_OF_SUB';
sub mkpath {
    my ($full_path, $perms, $opts) = @_;
    my $class = 'GT::File::Tools';
    $opts ||= {};

    my $untaint = delete $opts->{untaint};
    $untaint = $UNTAINT unless defined $untaint;

    my $untaint_regex = delete $opts->{untaint_regex};
    $untaint_regex = $REGEX unless defined $untaint_regex;

    $class->fatal(
        BADARGS => "Unknown option " . ( join ", ", keys %$opts )
    ) if keys %$opts;

    my ($path, $target, $is_relative) = parsefile($full_path);
    GT::File::Tools->fatal(BADARGS => 'You cannot pass a relative path to mkpath')
        if $is_relative;
    my $cwd = mycwd();
    if ($untaint) {
        $cwd =~ $untaint_regex and $cwd = $1;
        is_tainted($cwd) and die "bad file $cwd";
    }
    my @tomake = (split(m|/|, $path), $target);
    my $err = sub {
        my $bang = 0+$!;
        chdir $cwd;
        $! = $bang;
        $class->warn(@_) if @_;
        return;
    };

    # Find the deepest directory that exists, chdir into it, then mkdir all
    # remaining paths from that point on, chdir()ing, for performance reasons,
    # into each path as it is created.  This is necessary as permissions on
    # some OSes (Windows, and potentially unix systems with advanced
    # permissions) can have a path such as:
    # /foo/bar
    # where -e '/foo' is 0, but -e '/foo/bar' is 1

    my $start = '/';
    my @subpath; # /foo/bar/baz -> ('/foo/bar/baz/', '/foo/bar/', '/foo/', '/')
    for (reverse 0 .. $#tomake) {
        push @subpath, join '/', @tomake[0 .. $_], '';
    }
    SUBPATH: for my $i (0 .. $#subpath) {
        my $path = $subpath[$i];

        if ($untaint) {
            $path =~ $untaint_regex and $path = $1;
            is_tainted($path) and die "bad file $_";
        }

        if (-d $path) {
            return 1 if $i == 0; # The first path is the target
            $start = $path;
            splice @tomake, 0, -$i;
            last SUBPATH;
        }
    }

    chdir $start or return $err->("chdir: $!");

    for (@tomake) {
        next unless length;
        if ($untaint) {
            $_ =~ $untaint_regex and $_ = $1;
            is_tainted($_) and die "bad file $_";
        }
        if (!-d $_) {
            mkdir $_, 0777 or return $err->("mkdir $_: $!");
            if (defined $perms) {
                chmod $perms, $_ or return $err->("chmod: $!");
            }
        }
        chdir $_ or return $err->("chdir: $!");
    }
    chdir $cwd or return $err->("chdir $cwd: $!");
    return 1;
}
END_OF_SUB

$COMPILE{rmkdir} = __LINE__ . <<'END_OF_SUB';
# goto &foo didn't call AUTOLOAD until 5.005_03:
sub rmkdir { if ($] >= 5.005_03) { goto &mkpath } else { &mkpath } }
END_OF_SUB

$COMPILE{find} = __LINE__ . <<'END_OF_SUB';
sub find {
# ----------------------------------------------------------------------------
    my $class = 'GT::File::Tools';

    $class->fatal( BADARGS => "No arguments passed to find()" )
        unless @_;

    my $opts = ref $_[-1] eq 'HASH' ? pop : {};
    my $callback = pop;

    $class->fatal(
        BADARGS => "Argument after files list must be a code reference"
    ) unless ref $callback eq 'CODE';

    my $globbing = delete $opts->{globbing};
    $globbing = $GLOBBING unless defined $globbing;

    my @files = @_;
    @files = expand( @files ) if $globbing;

    $class->fatal( BADARGS => "No files to find" )
        unless @files;

    my $error_handler = delete $opts->{error_handler};
    $error_handler = sub { $class->warn( @_ ); 1 }
        unless defined $error_handler;

    $class->fatal(
        BADARGS => "error_handler option must be a code reference"
    ) unless ref $error_handler eq 'CODE';

    my $no_chdir = delete $opts->{no_chdir};
    $no_chdir = $NO_CHDIR unless defined $no_chdir;

    my $dirs_first = delete $opts->{dirs_first};
    $dirs_first = 1 unless defined $dirs_first;

    my $files_only = delete $opts->{files_only};
    $files_only = 0 unless defined $files_only;

    my $dirs_only = delete $opts->{dirs_only};
    $dirs_only = 0 unless defined $dirs_only;

    my $untaint = delete $opts->{untaint};
    $untaint = $UNTAINT unless defined $untaint;

    my $untaint_regex = delete $opts->{untaint_regex};
    $untaint_regex = $REGEX unless defined $untaint_regex;

    my $max_depth = delete $opts->{max_depth};
    $max_depth = $MAX_DEPTH unless defined $max_depth;

    $class->fatal(
        BADARGS => "You may only specify one of files_only or dirs_only"
    ) if $files_only and $dirs_only;

    $class->fatal(
        BADARGS => "Unknown option " . ( join ", ", keys %$opts )
    ) if keys %$opts;

    for my $path ( @files ) {
        if ($untaint) {
            $path =~ $untaint_regex and $path = $1;
            is_tainted($path) and die "bad file $path";
        }

        next unless -e $path;

        unless ( -d _ ) {
            $error_handler->( NOTADIR => $path ) or return;
            next;
        }

        my $relative = ( parsefile( $path ) )[2];
        my $cwd;
        if ( !$no_chdir or $relative ) {
            $cwd = mycwd();
            if ($untaint) {
                $cwd =~ $untaint_regex and $cwd = $1;
                is_tainted($cwd) and die "bad file $cwd";
            }
        }
        if ( $relative ) {
            $path = "$cwd/$path";
        }
        $class->debug( "find $path" ) if $DEBUG > 1;
        eval {
            _find( $path, $callback, {
                error_handler   => $error_handler,
                dirs_first      => $dirs_first,
                files_only      => $files_only,
                max_depth       => $max_depth,
                no_chdir        => $no_chdir,
                untaint         => $untaint,
                untaint_regex   => $untaint_regex,
                dirs_only       => $dirs_only
            }) or do {
                chdir $cwd unless $no_chdir;
                return;
            };
        };
        chdir $cwd unless $no_chdir;
        die "$@\n" if $@;
    }
    return 1;
}
END_OF_SUB

sub mycwd { getcwd || cwd || die "Could not get cwd; tried getcwd and cwd" }

$COMPILE{_find} = __LINE__ . <<'END_OF_SUB';
sub _find {
# ----------------------------------------------------------------------------
# This is so we can initialize from variable and cleanup in the main find
# function.
#
    my ( $path, $callback, $opts ) = @_;
    my $error_handler = $opts->{error_handler};
    local *DIR;
    if ( $opts->{dirs_first} and !$opts->{files_only} ) {
        $callback->( $path ) or return;
    }
    my $refs = 0;
    my $depth = 0;
    my $opened;
    if ( $opts->{no_chdir} ) {
        $opened = opendir DIR, $path;
    }
    else {
        if ( chdir $path ) {
            $opened = opendir DIR, ".";
        }
        else {
            $error_handler->( CHDIR => $path )
                or return;
        }
    }
    if ( $opened ) {
        my @files =
            map { s,/\Z,,; $opts->{no_chdir} ? "$path/$_" : $_ }
            grep { $_ ne '.' and $_ ne '..' } readdir DIR;
        closedir DIR;
        for ( my $i = 0; $i < @files; $i++ ) {
            my $file = $files[$i];

            if ( ref $file ) {
                if ($opts->{untaint}) {
                    $$file =~ $opts->{untaint_regex} and $$file = $1;
                    is_tainted($$file) and die "bad file $$file";
                }
                if ( !$opts->{dirs_first} and !$opts->{files_only} ) {
                    $callback->( $$file ) or return;
                }
                $depth-- if $opts->{max_depth};
                unless ( $opts->{no_chdir} ) {
                    chdir "..";
                    substr( $path, rindex($path, "/") ) = "";
                }
                next;
            }
            elsif ($opts->{untaint}) {
                $file =~ $opts->{untaint_regex} and $file = $1;
                is_tainted($file) and die "bad file $file";
            }

            if ( $opts->{max_depth} and $depth > $opts->{max_depth} ) {
                GT::File::Tools->fatal( 'TOODEEP' );
            }
            my $is_sym = -l $file;
            my $is_dir = -d $file;
            if ( $opts->{dirs_only} ) {
                next unless $is_dir;
            }
            if ($is_sym) {
                $callback->(  $opts->{no_chdir} ? $file : "$path/$file" ) or return;
            }
            elsif ( $is_dir ) {
                if ( $opts->{dirs_first} and !$opts->{files_only} ) {
                    $callback->( $opts->{no_chdir} ? $file : "$path/$file" ) or return;
                }
                local *DIR;
                $depth++;
                my @new_files;
                if ( $opts->{no_chdir} ) {
                    if ( opendir DIR, $file ) {
                        @new_files =
                            map { s,/\Z,,; "$file/$_" }
                            grep { $_ ne '.' and $_ ne '..' } readdir DIR;
                        closedir DIR;
                    }
                    else {
                        $error_handler->( OPENDIR => $file ) or return;
                    }
                }
                else {
                    my $opened;
                    if ( chdir $file ) {
                        $opened = opendir DIR, ".";
                    }
                    else {
                        $error_handler->( CHDIR => $file )
                            or return;
                    }
                    if ( $opened ) {
                        @new_files = map { s,/\Z,,; $_ } grep { $_ ne '.' and $_ ne '..' } readdir DIR;
                        closedir DIR;
                    }
                    else {
                        $error_handler->( OPENDIR => $file ) or return;
                    }
                    $path .= '/' . $file;
                }
                splice @files, $i + 1, 0, @new_files, ( $opts->{no_chdir} ? \$file : \$path );
            }
            else {
                next unless -e _;
                $callback->( $opts->{no_chdir} ? $file : "$path/$file" ) or return;
            }
        }
    }
    else {
        $error_handler->( OPENDIR => $path ) or return;
    }
    if ( !$opts->{dirs_first} and !$opts->{files_only} ) {
        $callback->( $path ) or return;
    }
    return 1;
}
END_OF_SUB

$COMPILE{_fix_symlink} = __LINE__ . <<'END_OF_SUB';
sub _fix_symlink {
# ----------------------------------------------------------------------------
# Tries to get the full path to what a symlink is pointing to. Returns the
# path (full or relative) and a value that is true if the path is relative and
# false otherwise.
#
    my ( $path ) = @_;
    my $link = readlink $path;
    my ( $relative1, $relative2 );
    ( undef, undef, $relative1 ) = parsefile( $link );
    ( undef, undef, $relative2 ) = parsefile( $path );
    if ( $relative1 and !$relative2 ) {
        $relative1 = 0;
        $link = dirname( $path ) . '/' . $link;
    }
    return ( $link, $relative1 );
}
END_OF_SUB

$COMPILE{_preserve} = __LINE__ . <<'END_OF_SUB';
sub _preserve {
# ----------------------------------------------------------------------------
# Set permissions, owner, mtime given file from, file to, and options:
#       set_time
#       set_owner
#       set_perms
#
    my ( $from, $to, %opts ) = @_;
    my $class = 'GT::File::Tools';

    my ( $mode, $uid, $gid, $mtime );
    if ( $opts{set_time} or $opts{set_owner} or $opts{set_perms} ) {
        ( $mode, $uid, $gid, $mtime ) = (stat($from))[2, 4, 5, 9];
    }
    if ( $opts{set_time} ) {
        utime time, $mtime, $to;
    }

    if ( $opts{set_owner} ) {
        chown $uid, $gid, $to
            if ( $> == 0 and $^O ne "MaxOS" and $^O ne "MSWin32" );
    }

    if ( $opts{set_perms} and !-l $to ) {
        chmod $mode, $to or return $class->warn( 'CHMOD', $to, "$!" );
    }
}
END_OF_SUB

$COMPILE{expand} = __LINE__ . <<'END_OF_SUB';
sub expand {
# ----------------------------------------------------------------------------
# Implement globbing for files. Perl's glob function has issues.
#
    my $class = 'GT::File::Tools';
    my ( @files ) = @_;
    my (@ret, $cwd);
    for ( @files ) {
        my ( $dirname, $filename, $relative ) = parsefile( $_ );
        if ($relative) {
            $cwd ||= mycwd();
            ($dirname, $filename) = parsefile( "$cwd/$_" );
        }
        if (
            index( $filename, '*' ) == -1 and
            index( $filename, '?' ) == -1
        )
        {
            push @ret, "$dirname/$filename";
            next;
        }
        $filename = quotemeta $filename;
        $filename =~ s[(^|\G|[^\\])((?:\\{4})*)\\(\\\\)?(\\(?!\\)|[?*])]{
            $1 . ('\\' x (length($2) / 2)) . ($3 ? "\\$4" : $4 eq '*' ? '.*' : $4 eq '?' ? '.' : '\\')
        }eg;
        local *DIR;
        opendir DIR, $dirname
            or $class->fatal( OPENDIR => $dirname, "$!" );
        push @ret, map "$dirname/$_", grep  { /\A$filename\Z/ and $_ ne '.' and $_ ne '..' } readdir DIR;
        closedir DIR;
    }
    return @ret;
}
END_OF_SUB


sub is_tainted { return ! eval { my $no_warn = join('',@_), kill 0; 1; } }

1;


} # End of BEGIN for GT/File/Tools.pm

BEGIN {
    $INC{"GT/Template.pm"} = "GT/Template.pm";

# ====================================================================
# Gossamer Threads Module Library - http://gossamer-threads.com/
#
#   GT::Template
#   Author: Jason Rhinelander
#   $Id: Template.pm,v 2.170 2008/08/22 18:42:17 scottm Exp $
#
# Copyright (c) 2005 Gossamer Threads Inc.  All Rights Reserved.
# ====================================================================
#
# Description:
#   A module for parsing templates.
#

package GT::Template;
# ===============================================================
use 5.004_04;
use strict;
use GT::Base();
use GT::CGI();
use GT::AutoLoader;
use vars qw(@ISA %FILE_CACHE %FILE_CACHE_PRINT $VERSION $DEBUG $ATTRIBS $ERRORS $PARSER $LAST_MODIFIED %CORE);
use constants
    MTIME => 9,
    INCLUDE_LIMIT => 15; # You're technically limited to double this number of includes as static and dynamic includes are counted separately.

@ISA     = qw/GT::Base/;
$VERSION = sprintf "%d.%03d", q$Revision: 2.170 $ =~ /(\d+)\.(\d+)/;
$DEBUG   = 0;
$ATTRIBS = {
    func_code         => undef,
    heap              => undef,
    root              => undef,
    include_root      => undef,
    varinc_allow_path => 0,
    strict            => 1,
    compress          => 0,
    begin             => '<%',
    end               => '%>',
    escape            => 0,
    print             => 0,
    stream            => 0,
    cache             => 1,
    indent            => '  ',
    dont_save         => 0,
    pkg_chop          => 1,
    disable           => undef,
    mtime             => undef
};
$ERRORS = {
    NOTEMPLATE           => "No template file was specified.",
    CANTOPEN             => "Unable to open template file '%s': %s",
    RENAME               => "Unable to rename '%s' to '%s': %s",
    NOTDIR               => "Error: '%s' is not a directory",
    CANTRUN              => "Unable to run compiled template file '%s': %s",
    CANTRUNSTRING        => "Unable to run compiled template code '%s' (from string): %s",
    CANTDIR              => "Unable to create compiled template directory '%s': %s",
    DIRNOTWRITEABLE      => "Compiled template directory '%s' is not writeable",
    LOOPNOTHASH          => "Error: Value '%s' for loop variable is not a hash reference",
    NOSUB                => "Error: No subroutine '%s' in '%s'",
    BADVAR               => "Error: Invalid variable name '\$%s' passed to function: %s::%s",
    CANTLOAD             => "Error: Unable to load module '%s': <blockquote>%s</blockquote>",
    NOTCODEREF           => "Error: Variable '%s' is not a code reference",
    CANTCALLCODE         => "Error: Unable to call '%s': %s",
    COMPILE              => "Error: Unable to compile function '%s': %s",
    UNKNOWNTAG           => "Unknown Tag: '%s'",
    TPLINFO_CANT_LOAD    => "Unable to read template information file '%s': %s",
    TPLINFO_CANT_COMPILE => "Unable to compile template information file '%s': %s",
    TPLINFO_NOT_HASH     => "Template information file '%s' does not contain a hash reference (Got '%s')",
    DISABLED_FUNC        => "Function calls have been disabled",
    DISABLED_FUNCARGS    => "Function calls with arguments have been disabled",
    DISABLED_FUNCRE      => "Function '%s' has been disabled",
    DISABLED_CODEARGS    => "Passing arguments to code reference variables has been disabled",
    DISABLED_ALIASARGS   => "Passing arguments to function aliases has been disabled",
    DISABLED_COREFUNCS   => "Core function calls have been disabled",
    BADINC               => "Error: Can't load included file '%s': %s",
    DEEPINC              => "Deep recursion in includes, aborting include!"
};
# Core perl functions that are callable as if they were GT::Template variables
# - these are only used if no other functions or variables override them.
%CORE = (
    substr  => sub { @_ > 2 ? substr($_[0], $_[1], $_[2]) : substr($_[0], $_[1]) },
    length  => sub { length($_[0]) },
    sprintf => sub { sprintf($_[0], @_[1 .. $#_]) },
    index   => sub { index($_[0], $_[1]) },
    rindex  => sub { rindex($_[0], $_[1]) },
    rand    => sub { rand($_[0]) },
    reverse => sub { reverse $_[0] },
    keys    => sub { [ keys(%{ $_[0] }) ] },
);


sub parse {
# -----------------------------------------------------------------------------
# Can be called as either a class method or object method.  When called as a
# class method, we need a new object (can't reuse due to function calls
# re-using the same parser).
#
    my $self = ref $_[0] ? shift : (shift->new);
    my ($template, $vars, $opt, $print) = @_; # The fourth argument should only be used internally.
    defined $template or exists $opt->{string} or return $self->fatal(NOTEMPLATE => $template);
    defined $vars or $vars  = {};
    defined $opt  or $opt   = {};

    my $alias = delete $opt->{alias};

# Set print if we were called via parse_print or parse_stream.
    if ($print and $print == 2 or $self->{stream} or $opt->{stream}) {
        $print = $self->{print} = $opt->{print} = 2;
    }
    elsif ($print or $self->{print} or $opt->{print}) {
        $print = $self->{print} = $opt->{print} = 1;
    }

    $self->{begin} = $opt->{begin} if $opt->{begin};
    $self->{end}   = $opt->{end}   if $opt->{end};

    $self->debug_level(delete $opt->{debug_level}) if exists $opt->{debug_level};

# Load the variables used in parsing.
    ref $vars eq 'ARRAY' ? $self->load_vars(@$vars) : $self->load_vars($vars);

# Load alias used for function calls.
    ref $alias eq 'ARRAY' ? $self->load_alias(@$alias) : $self->load_alias($alias) if $alias;

# Load the template which can either be a filename, or a string passed in.
    $self->{root}              = $opt->{root}              if defined $opt->{root};
    $self->{include_root}      = $opt->{include_root}      if defined $opt->{include_root};
    $self->{dont_save}         = $opt->{dont_save}         if defined $opt->{dont_save};
    $self->{pkg_chop}          = $opt->{pkg_chop}          if defined $opt->{pkg_chop};
    $self->{disable}           = $opt->{disable}           if ref $opt->{disable} eq 'HASH';
    $self->{varinc_allow_path} = $opt->{varinc_allow_path} if defined $opt->{varinc_allow_path};

    if (exists $opt->{string}) {
        $self->debug("Parsing string '$opt->{string}' with (print => @{[$opt->{print}||0]}, compress => @{[$opt->{compress}||0]}, strict => @{[$opt->{strict}||0]}, escape => @{[$opt->{escape}||0]})") if $self->{_debug};
        return $self->parse_string($opt->{string}, $opt);
    }

    if (not defined $self->{root}) {
        require File::Basename;
        $self->{root} = File::Basename::dirname($template);
        $template = File::Basename::basename($template);
    }

# Look for a template information file
    my $tplinfo = $self->load_tplinfo($self->{root});
    $self->{tplinfo} = $tplinfo if $tplinfo;

# Used to skip file modification checking on repeated dynamic includes:
    delete $self->{skip_mod_check};

    $self->load_template($template, $print);

# Parse the template.
    $self->debug("Parsing '$template' with (print => @{[$opt->{print}||0]}, compress => @{[$opt->{compress}||0]}, strict => @{[$opt->{strict}||0]}, escape => @{[$opt->{escape}||0]})") if $self->{_debug};

    $self->{mtime} = 0;
    if ($print and $print == 1) { # parse_print
        return print $self->_parse($template, $opt);
    }
    else { # parse or parse_stream
        return $self->_parse($template, $opt);
    }
}

sub parse_print {
# -----------------------------------------------------------------------------
# Print output rather than returning it.  Faster than parse_stream, but
# obviously, it does not stream.
#
    my $self = shift;
    $self->parse(@_[0 .. 2], 1);
}

$COMPILE{parse_stream} = __LINE__ . <<'END_OF_SUB';
sub parse_stream {
# -----------------------------------------------------------------------------
# Print output as template is parsed.  Only use if you really want streaming.
# Before using, you should probably set $| = 1, or you sort of defeat the whole
# point.
#
    my $self = shift;
    $self->parse(@_[0 .. 2], 2)
}
END_OF_SUB

$COMPILE{parse_string} = __LINE__ . <<'END_OF_SUB';
sub parse_string {
# -----------------------------------------------------------------------------
# Parses a string, only opts allowed is print mode on or off.  Internal use
# only.
#
    my ($self, $string, $opt) = @_;
    my $code = $self->_compile_string($string, $opt->{print});
    my $return = $code->($self);
    if ($opt->{print}) {
        return $opt->{print} == 2 ? $return : print $$return;
    }
    else {
        return $$return;
    }
}
END_OF_SUB

sub load_tplinfo {
# -----------------------------------------------------------------------------
# Returns the hash ref in the .tplinfo file.  Takes a single argument: The
# directory in which to look for a .tplinfo file (subdirectory "local" will be
# considered first, if it exists).
    my ($self, $root) = @_;
    my $tplinfo_file;

    if (-e "$root/local/.tplinfo") {
        $tplinfo_file = "$root/local/.tplinfo";
    }
    elsif (-e "$root/.tplinfo") {
        $tplinfo_file = "$root/.tplinfo";
    }
    if ($tplinfo_file) {
        local($!,$@);
        my $tplinfo = do $tplinfo_file;
        if (!$tplinfo) {
            $! and return $self->fatal(TPLINFO_CANT_LOAD => $tplinfo_file, "$!");
            $@ and return $self->fatal(TPLINFO_CANT_COMPILE => $tplinfo_file, "$@");
        }
        ref $tplinfo ne 'HASH' and return $self->fatal(TPLINFO_NOT_HASH => $tplinfo_file, "$tplinfo");
        return $tplinfo;
    }
    return;
}

sub load_template {
# -----------------------------------------------------------------------------
# Loads either a given filename, or a template string into the FILE_CACHE.
#
    my ($self, $file, $print) = @_;

# If this is a full root (either starts with / or c:, where c is any char), set
# the root and the filename appropriately.  We do this so that includes are
# relative to the directory that is being parsed.
    if (substr($file, 0, 1) eq '/' or substr($file, 1, 1) eq ':') {
        $self->{root} = substr($file, 0, rindex($file, '/'));
        substr($file, 0, rindex($file, '/') + 1) = '';
    }

# Get the full file name.
    my $full_file = $self->{root} . "/" . $file;
    my $this_file = $file;
    my $filename = $file;
    $filename =~ s|/|__|g;
    my $full_compiled = $self->{root} . "/compiled/" . $filename . ".compiled" . (($print and $print == 2) ? ".print" : "");

# Load from cache if we have it, otherwise load from disk.  If it's in the
# cache also make sure the file hasn't changed on disk.
    if ($self->{cache} and not $self->{dont_save}) {
        my $compiled;
        if (($print and $print == 2) ? (exists $FILE_CACHE_PRINT{$full_file}) : (exists $FILE_CACHE{$full_file})) {
            $self->debug("'$full_file' exists in the " . (($print and $print == 2) ? "parse_stream" : "parse") . " cache") if $self->{_debug};
            $compiled = 1;
        }
        elsif (-f $full_compiled and -r _) {
            local($@, $!);
            $full_compiled =~ /(.*)/;
            $full_compiled = $1;
            if ($print and $print == 2) {
                local $^W; # Prevent a "subroutine redefined" warning
                $FILE_CACHE_PRINT{$full_file} = do $full_compiled;
                $FILE_CACHE_PRINT{$full_file} and ($compiled = 1);
            }
            else {
                local $^W; # Prevent a "subroutine redefined" warning
                $FILE_CACHE{$full_file} = do $full_compiled;
                $FILE_CACHE{$full_file} and ($compiled = 1);
            }
            if (not $compiled and $self->{_debug}) {
                $self->debug("Could not compile template '$full_file'. Errors: \$\@: $@, \$!: $!");
            }
        }
        my ($files, $version);
        if ($compiled) {
            if ($print and $print == 2) {
                $files   = $FILE_CACHE_PRINT{$full_file}->{files} || [];
                $version = $FILE_CACHE_PRINT{$full_file}->{parser_version} || 0;
            }
            else {
                $files   = $FILE_CACHE{$full_file}->{files} || [];
                $version = $FILE_CACHE{$full_file}->{parser_version} || 0;
            }
            if ($version == $VERSION) {
                my $reload = 0;
                # Go through the template file list, looking for the final path of each one, then
                # compare path, size, and mtime with the cached value.
                require GT::Template::Inheritance;
                for (@$files) {
                    my ($file, $path, $mtime, $size) = @$_;
                    if ($file =~ m{^(?:[a-zA-Z]:)?[\\/]}) {
                        # An absolute path
                        if (-f $file and ((stat $file)[MTIME] != $mtime or -s _ != $size)) {
                            $self->debug("Recompiling '$full_file' because dependency '$file' has changed") if $self->{_debug};
                            $reload = 1; last;
                        }
                        next;
                    }
                    my $current = GT::Template::Inheritance->get_path(path => $self->{root}, file => $file);
                    if (not defined $current and defined $path) {
                        # File does not exist, but did when the cache was created
                        $self->debug("Recompiling '$full_file' because dependency '$file' no longer exists") if $self->{_debug};
                        $reload = 1; last;
                    }
                    if (defined $current and not defined $path) {
                        $self->debug("Recompiling '$full_file' because previously missing dependency '$file' now exists") if $self->{_debug};
                        $reload = 1; last;
                    }
                    if (defined $current and defined $path) {
                        if ($current ne $path) {
                            $self->debug("Recompiling '$full_file' because dependency '$file' has moved (now '$current', was '$path')") if $self->{_debug};
                            $reload = 1; last;
                        }
                        if (-f $current and ((stat $current)[MTIME] != $mtime or -s _ != $size)) {
                            $self->debug("Recompiling '$full_file' because dependency '$file' has changed") if $self->{_debug};
                            $reload = 1; last;
                        }
                    }
                }

                unless ($reload) {
                    $self->debug("'$full_file' does not need to be reloaded.  Using cached version.") if $self->{_debug};
                    return 1; # It doesn't need to be reloaded.
                }
            }
        }
        elsif ($self->{_debug}) {
            $self->debug("Compiling '$full_file' (compiled version does not exist or has an incorrect version)") if ($self->{_debug});
        }
    }

    if ($self->{dont_save}) {
        require GT::Template::Parser;
        my $parser = GT::Template::Parser->new(indent => $self->{indent}, begin => $self->{begin}, end => $self->{end});
        $parser->debug_level($self->{_debug}) if $self->{_debug};
        my ($eval) = $parser->parse(
            $this_file,
            {
                root => $self->{root},
                include_root => $self->{include_root}
            },
            ($print and $print == 2)
        );

# Check to see if the template name passed in is tainted.  If it's not tainted,
# we'll trust the data that's in the file and untaint $eval so that the eval
# below doesn't cause an insecure dependency error with taint mode.  This keeps
# things consistent with dont_save => 0.
        if (eval { eval("#" . substr($this_file, 0, 0)); 1 }) {
            ($$eval) = $$eval =~ /^(.*)$/s;
        }

        my $code;
        local ($@, $^W);
        eval { # Treat this like a string compilation
            eval "sub GT::Template::parsed_template { $$eval }";
            $code = \&GT::Template::parsed_template unless $@;
        };
        if (ref $code ne 'CODE') {
            return $self->fatal(CANTRUNSTRING => $$eval, "$@");
        }
        if ($print and $print == 2) {
            $FILE_CACHE_PRINT{$full_file} = { code => $code, dont_save => 1 };
        }
        else {
            $FILE_CACHE{$full_file} = { code => $code, dont_save => 1 };
        }
    }
    else {
# Needs to be reparsed for some reason (not in cache, old, etc.) so load it.
        if (not -e $self->{root} . "/compiled") {
            mkdir($self->{root} . "/compiled", 0777) or return $self->fatal(CANTDIR => "$self->{root}/compiled", "$!");
            chmod 0777, $self->{root} . "/compiled";
        }
        elsif (not -d _) {
            $self->fatal(NOTDIR => $self->{root} . "/compiled");
        }
        elsif (not -w _) {
            $self->fatal(DIRNOTWRITEABLE => "$self->{root}/compiled");
        }
        $self->_compile_template($this_file, $full_compiled, $print);

        local($@, $!);

        local $^W; # Prevent a "subroutine redefined" warning
        my $data = do $full_compiled or return $self->fatal(CANTRUN => $full_compiled, "\$\@: $@. \$!: $!");
        if ($print and $print == 2) { $FILE_CACHE_PRINT{$full_file} = $data }
        else                        { $FILE_CACHE{$full_file}       = $data }
    }
    return 1;
}

sub load_alias {
# ---------------------------------------------------------------
# Sets what aliases will be available in the template, can take a hesh,
# hash ref or a GT::Config object.
#
    my $self = shift;
    my $p = ref $_[0] ? shift() : {@_};

    $self->{ALIAS} ||= {};
    while ($p) {
        if (ref $p eq 'HASH' or UNIVERSAL::isa($p, 'GT::Config')) { # A GT::Config module (or subclass) is a tied hash.
            for (keys %$p) { $self->{ALIAS}->{$_} = $p->{$_} }
        }
        $p = shift;
    }
}

sub load_vars {
# ---------------------------------------------------------------
# Sets what variables will be available in the template, can take a hash,
# hash ref, cgi object, or a GT::Config object.
#
    my $self = shift;
    my $p = ref $_[0] ? shift() : {splice @_};

    $self->{VARS} ||= {};
    $self->{DELAY_VARS} ||= {};
    while ($p) {
        if (ref $p eq 'HASH') {
            for (keys %$p) {
                $self->{VARS}->{$_} = $p->{$_};
                delete $self->{DELAY_VARS}->{$_};
            }
        }
        elsif (UNIVERSAL::isa($p, 'GT::Config')) { # A GT::Config module (or subclass) is a tied hash.
            for (keys %$p) {
                $self->{VARS}->{$_} = undef;
                $self->{DELAY_VARS}->{$_} = $p;
# The DELAY_VARS key works to delay the loading of vars until we use them.  The primary purpose for this
# is speed - often GT::Template is used with a GT::Config object with compile_subs turned on - in such a
# case, reading the value from the hash would end up compiling the subroutine.  If the config file has
# 50 subroutines, and only 1 is used on the page, a lot of wasted processing time would occur without
# the delayed loading.  To do this, we store a reference to the Config object in DELAY_VARS, then if it
# is actually used we replace the VARS value with the real value/reference/etc.
            }
        }
        elsif (UNIVERSAL::can($p, 'param')) {
            for ($p->param) {
                $self->{VARS}->{$_} = $p->param($_);
                delete $self->{DELAY_VARS}->{$_};
            }
        }
        $p = shift;
    }
}

sub last_modified {
# -----------------------------------------------------------------------------
# Returns the last modified time of the most recent parse.  This is only
# accurate after the parse has finished, due to dynamic includes (which can be
# used as an optimization even when not explicitely using them).  Not available
# for string parsing (obviously).
#
    my $self = shift;
    return ref $self ? $self->{mtime} : $LAST_MODIFIED;
}

sub clear_vars {
# ---------------------------------------------------------------
# Clears the namespace.  Don't do this.
#
    %{$_[0]->{VARS}} = ();
    $_[0]->debug("Clearing internal variables.") if $_[0]->{_debug};
}

# This should only be called from functions that are called.  $PARSER is a
# localized reference of the current parser, and is used instead of $self when
# called as a class method.
sub vars {
    my $self = shift;
    $self = $PARSER if not ref $self;

    require GT::Template::Vars;
    tie my %tags, 'GT::Template::Vars', $self;

    return \%tags;
}

# This is deprecated in favour of ->vars.  See GT::Template::Vars.
sub tags { $PARSER->{VARS} }

$COMPILE{dump} = __LINE__ . <<'END_OF_SUB';
sub dump {
# -----------------------------------------------------------------------------
# Dumps the variables, used as a tag <%DUMP%> to display all tags available on
# the template.
#
    my %opts = @_;
    my $tags = GT::Template->vars;
    require GT::Dumper;
    my $dumper = GT::Dumper->new(sort => 1, var => '');
    my $output = '';
    if ($opts{-var}) {
        my $value = $tags->{$opts{-var}};
        my $html = not ($opts{-text} or ($opts{-auto} and not $ENV{GATEWAY_INTERFACE}));
        $output .= '<font face="Tahoma,Arial,Helvetica" size="2">' if $html;
        $output .= "Dumped value of '$opts{-var}':\n";
        $output .= '</font>' if $html;
        $output .= '<pre>' if $html;
        $output .= $dumper->dump(data => $value);
        $output .= '</pre>' if $html;
    }
    elsif ($opts{-text} or ($opts{-auto} and not $ENV{GATEWAY_INTERFACE})) {
        $output = "Available Variables\n";
        for my $key (sort keys %$tags) {
            my $val = $tags->{$key};
            $val = $$val if ref $val eq 'SCALAR' or ref $val eq 'LVALUE';
            $val = $dumper->dump(data => $val) if ref $val;
            local $^W;
            $output .= "$key => $val\n";
        }
    }
    else {
        my $font = 'font face="Tahoma, Arial, Helvetica" size="2"';
        $output = qq~<table border="1" cellpadding="3" cellspacing="0"><tr><td colspan="2"><$font><b>Available Variables</b></font></td></tr>~;
        for my $key (sort keys %$tags) {
            my $val = $tags->{$key};
            $val = $$val if ref $val eq 'SCALAR' or ref $val eq 'LVALUE';
            $val = $dumper->dump(data => $val) if ref $val;
            $val = GT::CGI::html_escape($val);
            local $^W;
            $val =~ s/ /&nbsp;/g;
            $val =~ s|\n|<br />\n|g;
            if ((not exists $opts{-hide_long} or $opts{-hide_long}) and (my $num_lines = $val =~ y/\n//) > 26) {
                my $id = join '', ('a'..'z', 'A'..'Z', 0..9)[map rand(62), 0 .. 24];
                my $more_lines = $num_lines - 25;
                $val =~ s{^((?:.*\n){25})}{$1<a href="#" onclick="document.getElementById('$id').style.display = 'block'; this.style.display = 'none'; return false" style="font-style: italic; text-decoration: underline">($more_lines more lines)</a><div id="$id" style="border: 0px; margin: 0px; padding: 0px; display: none">};
                $val .= "</div>";
            }
            $output .= qq~<tr><td valign="top"><$font>$key</font></td><td valign="top"><font face="Courier, Fixedsys">$val</font></td></tr>~;
        }
        $output .= qq~</table>~;
    }
    return \$output;
}
END_OF_SUB

sub _parse {
# ---------------------------------------------------------------
# Sets the parsing options, and gets the code ref and runs it.
#
    my ($self, $template, $opt) = @_;

    my $compress              = exists $opt->{compress}  ? $opt->{compress}  : $self->{compress};
    local $self->{opt}        = {};
    $self->{opt}->{strict}    = exists $opt->{strict}    ? $opt->{strict}    : $self->{strict};
    $self->{opt}->{print}     = exists $opt->{print}     ? $opt->{print}     : $self->{print};
    $self->{opt}->{escape}    = exists $opt->{escape}    ? $opt->{escape}    : $self->{escape};
    $self->{opt}->{package}   = exists $opt->{package}   ? $opt->{package}   : caller(1) || 'main';
    $self->{opt}->{func_code} = exists $opt->{func_code} ? $opt->{func_code} : $self->{func_code};
    $self->{opt}->{heap}      = exists $opt->{heap}      ? $opt->{heap}      : $self->{heap};

# Set the root if this is a full path so includes can be relative to template.
    if (substr($template, 0, 1) eq '/' or substr($template, 1, 1) eq ':') {
        $self->{root} = substr($template, 0, rindex($template, '/'));
        substr($template, 0, rindex($template, '/') + 1) = '';
    }
    my $root      = $self->{root};
    my $full_file = $self->{root} . '/' . $template;
    my ($code, $dont_save, $files) = $self->{opt}->{print} == 2
        ? @{$FILE_CACHE_PRINT{$full_file}}{qw/code dont_save files/}
        : @{$FILE_CACHE{$full_file}}{qw/code dont_save files/};

    # Determine the newest mtime from the cache info; this won't be accurate
    # until the template is completely parsed due to dynamic includes (which
    # may be used without your knowledge as an optimization).
    for (@$files) {
        my $mtime = $_->[2];
        $self->{mtime} = $mtime if $mtime and (!$self->{mtime} or $self->{mtime} < $mtime);
    }

    my $output = $code->($self);
    return $output if $self->{opt}->{print} == 2;

    $LAST_MODIFIED = $self->{mtime};

# Compress output if requested.
    if ($compress) {
        $self->debug("Compressing output for template '$template'.") if $self->{_debug};

        my ($pre_size, $post_size);
        $pre_size = length $$output if $self->{_debug};
        $self->_compress($output);
        $post_size = length $$output if $self->{_debug};

        $self->debug(sprintf "Output reduced %.1f%%.  Size before/after compression: %d/%d.", 100 * (1 - $post_size / $pre_size), $pre_size, $post_size) if $self->{_debug};
    }
    return $$output;
}

$COMPILE{_compile_template} = __LINE__ . <<'END_OF_SUB';
sub _compile_template {
# -------------------------------------------------------------------
# Loads the template parser and compiles the template and saves it
# to disk.
#
    my ($self, $file, $full_compiled, $print) = @_;
    $self->debug("Compiling template $file (into $full_compiled)") if $self->{_debug};
    require GT::Template::Parser;
    my $parser = GT::Template::Parser->new(indent => $self->{indent}, begin => $self->{begin}, end => $self->{end});
    $parser->debug_level($self->{_debug}) if $self->{_debug};

    my ($code, $files) = $parser->parse(
        $file,
        {
            root => $self->{root},
            include_root => $self->{include_root}
        },
        ($print and $print == 2)
    );

    local *FH;
    my $tmpfile = $full_compiled . "." . time . "." . $$ . "." . int(rand(10000)) . ".tmp";
    open FH, ">$tmpfile" or return $self->fatal(CANTOPEN => $tmpfile, "$!");
    my $localtime = localtime;
    my $file_string = '[' . join(',', map {
        my ($file, $path, $mtime, $size) = @$_;
        for ($file, $path) { s/([\\'])/\\$1/g if defined }
        "['$file'," . (defined $path ? "'$path'" : 'undef') . ",$mtime,$size]"
    } @$files) . ']';

    (my $escaped = $full_compiled) =~ s/(\W)/sprintf "_%x", ord($1)/ge;
    print FH qq
|# This file is a compiled version of a template that can be run much faster
# than reparsing the file, yet accomplishes the same thing.  You should not
# attempt to modify this file as any changes you make would be lost as soon as
# the original template file is modified.
# Editor: vim:syn=perl
# Generated: $localtime, using GT::Template::Parser v$GT::Template::Parser::VERSION
local \$^W;
{
    files => $file_string,
    parser_version => $VERSION,
    code => \\&GT::Template::parsed_template
};
sub GT::Template::parsed_template {
$$code
}|;
    close FH;
    unless (rename $tmpfile, $full_compiled) {
        unlink $tmpfile;
        return $self->fatal(RENAME => $tmpfile, $full_compiled, "$!");
    }
    chmod 0666, $full_compiled;
    return;
}
END_OF_SUB

$COMPILE{_compile_string} = __LINE__ . <<'END_OF_SUB';
sub _compile_string {
# -----------------------------------------------------------------------------
# Like _compile_template, except that this returns a code reference for the
# passed in string.
# Takes two arguments: The string, and print mode.  If print mode is on, the
# code will print everything and return 1, otherwise the return will be the
# result of the template string.
    my ($self, $string, $print) = @_;
    $self->debug("Compiling string '$string' in " . (($print and $print == 2) ? "stream mode" : "return mode")) if $self->{_debug};
    if (!$string) {
        $self->debug("Actual parsing skipped for empty or false string '$string'") if $self->{_debug};
        if ($print and $print == 2) {
            return sub { print $string };
        }
        else {
            return sub { \$string };
        }
    }

    require GT::Template::Parser;
    my $parser = GT::Template::Parser->new(indent => $self->{indent}, begin => $self->{begin}, end => $self->{end});
    $parser->debug_level($self->{_debug}) if $self->{_debug};
    my ($eval) = $parser->parse(
        $string,
        {
            root => $self->{root},
            include_root => $self->{include_root},
            string => $string
        },
        ($print and $print == 2)
    );

    my $code;
    local ($@, $^W);
    eval { # Catch tainted data
        eval "sub GT::Template::parsed_template { $$eval }";
        $code = \&GT::Template::parsed_template unless $@;
    };

    unless (ref $code eq 'CODE') {
        return $self->fatal(CANTRUNSTRING => "sub GT::Template::parsed_template { $$eval }", "$@");
    }
    return $code;
}
END_OF_SUB

$COMPILE{_call_func} = __LINE__ . <<'END_OF_SUB';
sub _call_func {
# -----------------------------------------------------------------------------
# Calls a function.  The arguments are set in GT::Template::Parser.  If the
# function returns a hash, it is added to $self->{VARS} _unless_ the 'set'
# option is provided and true.  The result of the function is escaped, if
# escape mode is turned on.
#
    my ($self, $torun, $allow_strict, $set, @args) = @_;
    my $aliased;
    if (exists $self->{ALIAS}->{$torun}) {
        $torun = $self->{ALIAS}->{$torun};
        $aliased = 1;
    }
    no strict 'refs';
    my $rindex = rindex($torun, '::');
    my $package;
    $package = substr($torun, 0, $rindex) if $rindex != -1;
    my ($code, $ret);
    my @err = ();
    my $ok  = 0;
    if ($package) {
        my $disabled;
        if ($aliased) {
            if ($self->{disable}->{alias_args} and @args) {
                $disabled = $ERRORS->{DISABLED_ALIASARGS};
            }
        }
        elsif ($self->{disable}->{functions}) {
            $disabled = $ERRORS->{DISABLED_FUNC};
        }
        elsif ($self->{disable}->{function_args} and @args) {
            $disabled = $ERRORS->{DISABLED_FUNCARGS};
        }
        elsif ($self->{disable}->{function_restrict} and $torun !~ /$self->{disable}->{function_restrict}/) {
            $disabled = sprintf $ERRORS->{DISABLED_FUNCRE}, $torun;
        }

        if ($disabled) {
            push @err, $disabled;
        }
        else {
            my $func = substr($torun, rindex($torun, '::') + 2);
            (my $pkg = $package) =~ s,::,/,g;
            until ($ok) {
                local ($@, $SIG{__DIE__});
                my $req = eval { require "$pkg.pm" };
                if (!$req) {
                    push @err, $@;
                    # Remove file from %INC so that future require's don't succeed:
                    delete $INC{"$pkg.pm"};
                }
                elsif (defined(&{$package . '::' . $func})
                    or defined &{$package . '::AUTOLOAD'} and defined %{$package . '::COMPILE'} and exists ${$package . '::COMPILE'}{$func}
                ) {
                    $ok = 1;
                    $code = \&{$package . '::' . $func};
                    last;
                }
                else {
                    push @err, sprintf($ERRORS->{NOSUB}, "$package\::$func", "$pkg.pm");
                }
                my $pos = rindex($pkg, '/');
                $pos == -1 ? last : (substr($pkg, $pos) = "");
                last unless $self->{pkg_chop};
            }
        }
    }
    elsif (ref $self->{VARS}->{$torun} eq 'CODE') {
        if ($self->{disable}->{coderef_args} and @args) {
            push @err, $ERRORS->{DISABLED_CODEARGS};
        }
        else {
            $code = $self->{VARS}->{$torun};
            $ok = 1;
        }
    }
    elsif ($self->{DELAY_VARS}->{$torun}) {
        if ($self->{disable}->{coderef_args} and @args) {
            push @err, $ERRORS->{DISABLED_CODEARGS};
        }
        else {
            $code = $self->{VARS}->{$torun} = $self->{DELAY_VARS}->{$torun}->{$torun};
            delete $self->{DELAY_VARS}->{$torun};
            $ok = 1;
        }
    }
    elsif ($CORE{$torun}) {
        if ($self->{disable}->{core_functions}) {
            push @err, $ERRORS->{DISABLED_COREFUNCS};
        }
        else {
            $code = $CORE{$torun};
            $ok = 1;
        }
    }

    if ($ok) {
        local $PARSER = $self;
        if ($self->{opt}->{heap}) {
            push @args, $self->{opt}->{heap}
        }
        if ($package and ref($self->{opt}->{func_code}) eq 'CODE') {
            $ret = $self->{opt}->{func_code}->($torun, @args);
        }
        else {
            $ret = $code->(@args);
        }
        if (ref $ret eq 'HASH' and not $set) {
            my $tags = $self->vars;
            @$tags{keys %$ret} = values %$ret;
            $ret = '';
        }
    }
    elsif ($package) {
        $ret = ($allow_strict and $self->{opt}->{strict}) ? \sprintf($ERRORS->{CANTLOAD}, $package, join(",<br />\n", @err)) : '';
    }
    else {
        if (@err) {
            $ret = ($allow_strict and $self->{opt}->{strict}) ? \sprintf($ERRORS->{CANTCALLCODE}, $torun, join(",<br />\n", @err)) : '';
        }
        else {
            $ret = ($allow_strict and $self->{opt}->{strict}) ? \sprintf($ERRORS->{NOTCODEREF}, $torun) : '';
        }
    }

    $ret = '' if not defined $ret;
    $ret = (ref $ret eq 'SCALAR' or ref $ret eq 'LVALUE') ? $$ret : ($set and ref $ret) ? $ret : $self->{opt}->{escape} ? GT::CGI::html_escape($ret) : $ret;
    return $ret;
}
END_OF_SUB

$COMPILE{_compress} = __LINE__ . <<'END_OF_SUB';
sub _compress {
# -----------------------------------------------------------------------------
# Compress html by removing extra space (idea/some re from HTML::Clean).
# Avoids compressing pre tags.
#
    my ($self, $text) = @_;
    if ($$text =~ /<pre\b/i or $$text =~ /<textarea\b/i) {
        $$text .= "<pre></pre>";
        $$text =~ s(\G(.*?)(<\s*(pre|textarea)\b.*?<\s*/\3\s*>))(
            my $html = $1;
            my $pre  = $2 || '';
            $html =~ s/\s+\n/\n/g;
            $html =~ s/\n\s+</\n</g;
            $html =~ s/\n\s{2,}/\n /g;
            $html =~ s/>\s{2,}</> </g;
            $html =~ s/\s+>/>/g;
            $html =~ s/<\s+/</g;
            $html . $pre;
        )iesg;
        substr($$text, -11) = '';
    }
    else {
        $$text =~ s/\s+\n/\n/g;
        $$text =~ s/\n\s+</\n</g;
        $$text =~ s/\n\s{2,}/\n /g;
        $$text =~ s/>\s{2,}</> </g;
        $$text =~ s/\s+>/>/g;
        $$text =~ s/<\s+/</g;
    }
    return $text;
}
END_OF_SUB

sub _get_var {
# -----------------------------------------------------------------------------
# Returns the string value of a variable.  If it's a hash, it adds the
# variables to the current tags, and returns undef.
# It takes 2 arguments - the "thing" to check, and a hash ref of options, where
# options are:
#   - escape (default off) - whether to apply HTML escaping for non-ref vars
#   - strict (default off) - whether to return "Unknown tag '...'" instead of
#                            undef for non-existent variables
#   - merge (default on) - if variable is a hash ref, whether to merge values
#                          and return undef (true) or not merge and return 1
#                          (false).  The latter is used in if tags.
#   - return_ref (default off) - mainly used for sets - if the right side
#                                variable is a reference, then return the
#                                reference instead of a value.
#
    my ($self, $str, $opt) = @_;

# Backwards compatibility with old compiled files generated by
# GT::Template::Parser <= r2.151
    $opt = { escape => $_[2], strict => $_[3] } if not ref $opt and defined $opt;

    $opt ||= { escape => 0, strict => 0 };
    $opt->{merge} = 1 if not exists $opt->{merge};
    $opt->{return_ref} = 0 unless $opt->{return_ref};

    my ($ret, $good) = ('', 1);
    if (ref($str) eq 'HASH') {
        $ret = $str;
    }
    elsif (exists $self->{ALIAS}->{$str}) {
        $ret = $self->_call_func($str);
    }
    elsif (my ($val) = $self->_raw_value($str)) {
        if (ref $val eq 'CODE') {
            local $PARSER = $self;
            $ret = $val->($self->vars, $self->{opt}->{heap} || ());

            $ret = '' if not defined $ret;
        }
        else {
            $ret = $val;
            $ret = '' if not defined $ret;
        }
    }
    elsif ($str eq 'TIME') {
        return time;
    }
    else {
        $good = 0;
    }

    if (not $good) {
        return $opt->{strict} ? sprintf($ERRORS->{UNKNOWNTAG}, $str) : undef;
    }
    if ($opt->{return_ref} and (ref $ret eq 'HASH' or ref $ret eq 'ARRAY')) {
        return $ret;
    }
    if (ref $ret eq 'HASH') {
        return 1 if not $opt->{merge};
        my $tags = $self->vars;
        @$tags{keys %$ret} = values %$ret;
        return;
    }
    return if not defined $ret;
    return $$ret if ref $ret eq 'SCALAR' or ref $ret eq 'LVALUE';
    return $ret if not $opt->{escape};
    $ret =~ s/&/&amp;/g;
    $ret =~ s/</&lt;/g;
    $ret =~ s/>/&gt;/g;
    $ret =~ s/"/&quot;/g;
    return $ret;
}

sub _raw_value {
# -----------------------------------------------------------------------------
# Gets a raw value.  If the variable doesn't exist, returns an empty list (or
# undef, in scalar context).
#
    my ($self, $key) = @_;
    if (exists $self->{VARS}->{$key} and $self->{DELAY_VARS}->{$key}) {
        $self->{VARS}->{$key} = $self->{DELAY_VARS}->{$key}->{$key};
        delete $self->{DELAY_VARS}->{$key};
    }
    return $self->{VARS}->{$key} if exists $self->{VARS}->{$key};
    return time if $key eq 'TIME';

    if ($key =~ /^\w+(?:\.\$?\w+)+$/) {
        my $cur = $self->{VARS};
        my @k = split /\./, $key;
        for (my $i = 0; $i < @k; $i++) {
            if ($k[$i] =~ /^\$/) {
                my $val = $self->_get_var(substr($k[$i], 1));
                $val = '' if not defined $val;
                my @pieces = split /\./, $val;
                @pieces = '' if !@pieces;
                splice @k, $i, 1, @pieces;
                $i += @pieces - 1 if @pieces > 1;
            }
        }
        KEY: while (@k) {
            # for a.b.c:
            # @k = ('a', 'b', 'c')
            # @i = ('a.b.c', 'a.b', 'a')
            # This is needed because "a.b.c" will look for key "b.c" in hash "a" before key "b"
            my @i = map join('.', @k[0 .. $_]), reverse 1 .. $#k;
            push @i, shift @k;

            {
                if (ref $cur eq 'CODE') {
                    # current node (e.g. a.b for a.b.c) is a code ref; call it, and try again
                    $cur = $cur->($self->{opt}->{heap} || ());
                    redo;
                }
                elsif (ref $cur eq 'ARRAY' and $i[-1] =~ /^\d+$/) {
                    return if $i[-1] > $#$cur;
                    $cur = $cur->[$i[-1]];
                }
                elsif (ref $cur eq 'ARRAY' and $i[-1] =~ /^last(\d+)?$/) {
                    my $negi = $1 || 1;
                    return if $negi > @$cur;
                    $cur = $cur->[-$negi];
                }
                elsif (!@k and ref $cur eq 'ARRAY' and $i[0] eq 'length') {
                    $cur = scalar @$cur;
                }
                elsif (ref $cur eq 'HASH' or UNIVERSAL::isa($cur, 'GT::Config')) {
                    my $exists;
                    for (0 .. $#i) {
                        if (exists $cur->{$i[$_]}) {
                            splice @k, 0, $#i-$_ unless $_ == $#i;
                            $cur = $cur->{$i[$_]};
                            $exists = 1;
                            last;
                        }
                    }
                    return unless $exists;
                }
                elsif (UNIVERSAL::can($cur, 'param') and my ($val) = $cur->param($i[0])) {
                    $cur = $val;
                    last KEY;
                }
                else {
                    return;
                }
            }
        }

        return $cur;
    }

    return;
}

sub _include {
# -----------------------------------------------------------------------------
# Perform a runtime include of a file.
#
    my ($self, $template, $allow_path) = @_;

    $allow_path = $self->{varinc_allow_path} unless defined $allow_path;

    if ($template eq '.' or $template eq '..' or ($template =~ m{[/\\]} and !$allow_path)) {
        return sprintf $ERRORS->{BADINC}, $template, 'Invalid characters in filename';
    }

    if (++$self->{include_safety} > GT::Template::INCLUDE_LIMIT) {
        return $ERRORS->{DEEPINC};
    }

    if ($allow_path and $self->{include_root} and $template =~ m{^(?:[a-zA-Z]:)?[/\\]}) {
        # Remove the drive letter on Windows
        $template =~ s/^[a-zA-Z]://;
        $template = $self->{include_root} . $template;

# A small (hopefully temporary) hack to fix the problem where the compiled
# files end up in the included template's directory.
        if ($self->{root}) {
            $template =~ s|^\Q$self->{root}\E[/\\]||;
        }
    }

    my $opt = $self->{opt};
    my $print = $self->{print};
    my $streaming = $print && $print == 2;
    $self->load_template($template, $streaming ? 2 : 0) unless $self->{skip_mod_check}->{$template}++;

    $self->debug("Parsing dynamic include '$template' with (print => @{[$opt->{print}||0]}, compress => @{[$opt->{compress}||0]}, strict => @{[$opt->{strict}||0]}, escape => @{[$opt->{escape}||0]})") if $self->{_debug};

    my $ret = $self->_parse($template, $opt);

    --$self->{include_safety};

    return $streaming ? '' : $ret || '';
}

1;


} # End of BEGIN for GT/Template.pm

BEGIN {
    $INC{"GT/Template/Parser.pm"} = "GT/Template/Parser.pm";

# ====================================================================
# Gossamer Threads Module Library - http://gossamer-threads.com/
#
#   GT::Template::Parser
#   Author: Jason Rhinelander
#   $Id: Parser.pm,v 2.159 2008/08/16 22:48:22 brewt Exp $
#
# Copyright (c) 2005 Gossamer Threads Inc.  All Rights Reserved.
# ====================================================================
#
# Description:
#   A module for parsing templates. This module actually generates
#   Perl code that will print the template.
#

package GT::Template::Parser;
# ===============================================================

use 5.004_04;
use strict;

use GT::Base;
use GT::Template;

use vars qw(@ISA $VERSION $DEBUG $ATTRIBS $ERRORS %ESCAPE_MAP);

@ISA     = qw/GT::Base/;
$VERSION = sprintf "%d.%03d", q$Revision: 2.159 $ =~ /(\d+)\.(\d+)/;
$DEBUG   = 0;
$ATTRIBS = {
    root         => '.',
    include_root => '',
    indent       => '  ',
    begin        => '<%',
    end          => '%>',
    print        => 0
};
$ERRORS  = {
    NOTEMPLATE        => "No template file was specified.",
    BADINC            => $GT::Template::ERRORS->{BADINC},
    CANTOPEN          => "Unable to open template file '%s': %s",
    DEEPINC           => $GT::Template::ERRORS->{DEEPINC},
    EXTRAELSE         => "Error: extra else tag",
    EXTRAELSIF        => "Error: extra elsif/elseif tag",
    NOSCALAR          => "Error: Variable '%s' is not scalar",
    UNMATCHEDELSE     => "Error: Unmatched else tag",
    UNMATCHEDELSIF    => "Error: Unmatched elsif/elseif tag",
    UNMATCHEDENDIF    => "Error: Unmatched endif/endifnot/endunless tag",
    UNMATCHEDENDLOOP  => "Error: endloop found outside of loop",
    UNMATCHEDNEXTLOOP => "Error: nextloop found outside of loop",
    UNMATCHEDLASTLOOP => "Error: lastloop found outside of loop",
    UNKNOWNTAG        => $GT::Template::ERRORS->{UNKNOWNTAG},
    UNKNOWNINCLUDETAG => "Unknown tag in include: '%s'"
};

use vars qw/%FILTERS $RE_FILTERS $RE_SET $RE_MATH $RE_EXPR/;

%FILTERS = (
    escape_html   => '$tmp = GT::CGI::html_escape($tmp);',
    unescape_html => '$tmp = GT::CGI::html_unescape($tmp);',
    escape_url    => '$tmp = GT::CGI::escape($tmp);',
    unescape_url  => '$tmp = GT::CGI::unescape($tmp);',
    escape_js     => q{$tmp =~ s{([\\\/'"])}{\\\$1}g; $tmp =~ s{(?:\r\n|\r|\n)}{\\\n}g;},
    nbsp          => '$tmp =~ s/\s/&nbsp;/g;'
);
@FILTERS{qw/escapeHTML unescapeHTML escapeURL unescapeURL escapeJS/} = @FILTERS{qw/escape_html unescape_html escape_url unescape_url escape_js/};
for (qw/uc lc ucfirst lcfirst/) {
    $FILTERS{$_} = '$tmp = ' . $_ . '($tmp);';
}
$RE_FILTERS = '(?:(?:' . join('|', map quotemeta, keys %FILTERS) . ')\b\s*)+';

$RE_SET = q(set\s+(\w+(?:\.\$?\w+)*)\s*([-+*/%^.]|\bx|\|\||&&)?=\s*); # Two captures - the variable and the (optional) assignment modifier
$RE_EXPR = qq{($RE_FILTERS)?('(?:[^\\\\']|\\\\.)*'|"(?:[^\\\\"]|\\\\.)*"|(?!$RE_FILTERS)[^\\s('"]+)}; # Two captures - the (optional) filters, and the value/variable
$RE_MATH = q(\bx\b|/\d+(?=\s)|\bi/|[+*%~^/-]|\|\||&&);

sub parse {
# ---------------------------------------------------------------
# Can be called as either a class method or object method. This
# returns three things - the first is a scalar reference to a string
# containing all the perl code, the second is an array reference
# of dependencies, and the third is the filetype of the template -
# matching this regular expression:  /^((INH:)*(REL|LOCAL)|STRING)$/.
# For example, 'INH:INH:INH:INH:LOCAL', 'LOCAL', 'INH:REL', 'REL', or 'STRING'
#
    my $self = ref $_[0] ? shift : (shift->new);
    my ($template, $opt, $print) = @_; # The third argument should only be used internally.
    defined $template or return $self->fatal(NOTEMPLATE => $template);
    defined $opt      or $opt  = {};

# Set print to 1 if we were called via parse_print.
    $opt->{print} = 1 if $print;

# Load the template which can either be a filename, or a string passed in.
    $self->{root} = $opt->{root} if $opt->{root};
    $self->{include_root} = $opt->{include_root} if $opt->{include_root};

    my ($full, $string);
    my $type = '';
    if (exists $opt->{string}) {
        $full = $template;
        $string = $opt->{string};
        $type = "STRING";
    }
    else {
        require GT::Template::Inheritance;
        $full = GT::Template::Inheritance->get_path(path => $self->{root}, file => $template)
            or return $self->fatal(CANTOPEN => $template, "File does not exist.");
    }

    my ($mtime, $size, $tpl) = (0, 0);
    if (defined $string) {
        $tpl = \$string;
    }
    else {
        ($mtime, $size, $tpl) = $self->load_template($full);
    }

# Parse the template.
    $self->debug("Parsing '$template' (found '$full') with (print => $opt->{print})") if $self->{_debug};
    my @files = ([$template, $full, $mtime, $size]);
    my $code = $self->_parse($template, $opt, $tpl, \@files);

# Return the code, and an array reference of [filename, path, mtime, size] items
    return ($code, \@files);
}

sub parse_print {
# ---------------------------------------------------------------
# Print output as template is parsed.
#
    my $self = shift;
    $self->parse(@_[0..1], 1)
}

sub load_template {
# ---------------------------------------------------------------
# Loads either a given filename, or a template string, and returns a reference to it.
#
    my ($self, $full_file) = @_;

    $self->debug("Reading '$full_file'") if $self->{_debug};

    -e $full_file or return $self->fatal(CANTOPEN => $full_file, "File does not exist.");
    local *TPL;
    open TPL, "< $full_file" or return $self->fatal(CANTOPEN => $full_file, "$!");
    my ($mtime, $size) = (stat TPL)[9, 7];
    my $ret = \do { local $/; <TPL> };
    close TPL;

    return $mtime, $size, $ret;
}

sub _parse {
# ---------------------------------------------------------------
# Parses a template.
#
    my ($self, $template, $opt, $tpl, $files) = @_;

    local $self->{opt}     = {};
    $self->{opt}->{print}  = exists $opt->{print}  ? $opt->{print}  : $self->{print};
    $self->{opt}->{indent} = exists $opt->{indent} ? $opt->{indent} : $self->{indent};

    unless (defined $opt->{string}) {
# Set the root if this is a full path so includes can be relative to template.
        if ((not $self->{root} or $self->{root} eq '.') and ((index($template, '/') == 0) or (index($template, ':') == 1))) {
            $self->{root} = substr($template, 0, rindex($template, '/'));
            substr($template, 0, rindex($template, '/') + 1) = '';
        }
    }

    return $self->_parse_tags($tpl, $files);
}

sub _text_escape {
    my $text = shift;
    $text =~ s/(\\(?=[{}\\]|$)|[{}])/\\$1/g;
    $text;
}

sub _filter {
    my ($filter, $var) = @_;
    my $f = $FILTERS{$filter};
    $f =~ s/\$tmp\b/$var/g if $var;
    $f . " # $filter";
}

sub _comment {
    my $comment = shift;
    $comment =~ s/^/#/gm;
    $comment . "\n";
}

sub _parse_tags {
# ---------------------------------------------------------------
# Returns a string containing perl code that, when run (the code should be
# passed a template object as its argument) will produce the template.
# Specifically, the returned from this is a scalar reference (containing the
# perl code) and an array reference of the file's dependencies.
#
    my ($self, $tplref, $files) = @_;

    my $tpl = $$tplref;

    my $begin      = quotemeta($self->{begin});
    my $end        = quotemeta($self->{end});
    my $root       = $self->{root};
    my $loop_depth = 0;
    my $i          = -1;
    my @seen_else  = ();
    my @if_level   = ();
    my $print      = $self->{opt}->{print};
    my $indent       = $self->{opt}->{indent};
    my $indent_level = 0; # The file is already going to be in a hash

    my %deps;

    my $last_pos = 0;

# Can only go up to GT::Template::INCLUDE_LIMIT includes inside includes.
    my $include_safety  = 0;
# Store the "if" depth so that too many or too few <%endif%>'s in an include
# won't break things:
    my @include_ifdepth;

    my $return          = <<'CODE';

local $^W; # Get rid of warnings.  This won't work for Perl 5.6's -W switch
my $self = shift;
my $return = '';
my $tags = $self->vars;
my $escape = $self->{opt}->{escape};
my $strict = $self->{opt}->{strict};
my ($tmp, $tmp2, $tmp3);
CODE

# We loop through the text looking for <% and %> tags, but also watching out for comments
# <%-- some comment --%> as they can contain other tags.
    my $text = sub {
        my $text = shift;
        length $text or return;
        $return .= ($indent x ($indent_level)) . ($print ? q|print q{| : q|$return .= q{|);
        $return .= _text_escape($text) . q|};
|;  };

    #               $1                                                  $2
    while ($tpl =~ /(\s*$begin\s*~\s*$end\s*|(?:\s*$begin\s*~|$begin)\s*(--.*?(?:--(?=\s*(?:~\s*)?$end)|$)|.+?)\s*(?:~\s*$end\s*|$end|$))/gs) {
        my $tag = $2;
        my $tag_len     = length $1;
        my $print_start = $last_pos;
        $last_pos       = pos $tpl;
        # Print out the text before the tag.
        $text->(substr($tpl, $print_start, $last_pos - $tag_len - $print_start));

        next unless defined $tag; # Won't be defined for: <%~%>, which is a special cased no-op, whitespace reduction tag

# Handle nested comments
        if (substr($tag,0,2) eq '--') {
            my $save_pos = pos($tag);
            while ($tag =~ /\G.*?$begin\s*(?:~\s*)?--/gs) {
                $save_pos = pos($tag);
                my $tpl_save_pos = pos($tpl);
                if ($tpl =~ /\G(.*?--\s*(?:~\s*$end\s*|$end))/gs) {
                    $tag .= $1;
                    pos($tag) = $save_pos;
                    $last_pos = pos($tpl);
                }
                else {
                    $last_pos = pos($tpl) = length($tpl);
                    $tag .= substr($tpl, $last_pos);
                    last;
                }
            }
        }
# Tag consists of only \w's and .'s - it's either a variable or some sort of
# keyword (else, endif, etc.)
        elsif ($tag !~ /[^\w.]/) {

# 'else' - If $i is already at -1, we have an umatched tag.
            if ($tag eq 'else') {
                if ($i == -1 or $indent_level != $if_level[$i]) {
                    $return .= _comment($ERRORS->{UNMATCHEDELSE});
                    $text->($ERRORS->{UNMATCHEDELSE});
                }
                elsif ($seen_else[$i]++) {
                    $return .= _comment($ERRORS->{EXTRAELSE});
                    $text->($ERRORS->{EXTRAELSE});
                }
                else {
                    $return .= $indent x ($indent_level - 1) . q|}
|;                  $return .= $indent x ($indent_level - 1) . q|else {
|;              }
            }

# 'endif', 'endunless', 'endifnot' - decrement our level. If $i is already at -1, we have an umatched tag.
            elsif ($tag eq 'endif' or $tag eq 'endifnot' or $tag eq 'endunless') {
                if ($i == -1 or @include_ifdepth and $i <= $include_ifdepth[-1][0] or $indent_level != $if_level[$i]) {
                    $return .= _comment($ERRORS->{UNMATCHEDENDIF});
                    $text->($ERRORS->{UNMATCHEDENDIF});
                }
                else {
                    --$i; --$#seen_else; --$#if_level; # for vim: {
                    $return .= $indent x --$indent_level . q|}
|;              }
            }
# 'endloop' - ends a loop
            elsif ($tag eq 'endloop') {
                if ($loop_depth <= 0) {
                    $return .= _comment($ERRORS->{UNMATCHEDENDLOOP});
                    $text->($ERRORS->{UNMATCHEDENDLOOP});
                }
                else {
                    $loop_depth--; # for vim: {{{{
                    $return .= $indent x --$indent_level . q|}
|;                  $return .= $indent x --$indent_level . q|}
|;                  $return .= $indent x --$indent_level . q|}
|;                  $return .= $indent x $indent_level . q|for (keys %loop_set) { $self->{VARS}->{$_} = $orig->{$_} }
|;                  $return .= $indent x --$indent_level . q|}
|;              }
            }
# 'lastloop' - simply put in a last;
            elsif ($tag eq 'lastloop') {
                if ($loop_depth <= 0) {
                    $return .= _comment($ERRORS->{UNMATCHEDLASTLOOP});
                    $text->($ERRORS->{UNMATCHEDLASTLOOP});
                }
                else {
                    $return .= $indent x $indent_level . q|last LOOP| . $loop_depth . q|;
|;              }
            }
# 'nextloop' - simply put in a next;
            elsif ($tag eq 'nextloop') {
                if ($loop_depth <= 0) {
                    $return .= _comment($ERRORS->{UNMATCHEDNEXTLOOP});
                    $text->($ERRORS->{UNMATCHEDNEXTLOOP});
                }
                else {
                    $return .= $indent x $indent_level . q|next;
|;              }
            }
# 'endparse' - stops the parser.
            elsif ($tag eq 'endparse') {
                $return .= $indent x $indent_level . q|return | . ($print ? q|1| : q|\$return|) . q|;
|;          }
# 'endinclude' - this is put at the end of an include when the include is inserted into the current template data.
            elsif ($tag eq 'endinclude') {
                if (@include_ifdepth) {
                    while ($indent_level > $include_ifdepth[-1][1]) { # for vim: {
                        $return .= ($indent x --$indent_level) . q|}
|;                  }
                    $i = $include_ifdepth[-1][0];
                }
                $include_safety--;
                pop @include_ifdepth; # for vim: {
                $return .= $indent x --$indent_level . q|} # Done include
|;          }
            elsif ($tag eq 'DUMP') {
                my $func = $self->_check_func('GT::Template::dump(-auto => 1)', 1);
                $return .= ($indent x $indent_level) . ($print ? q|print | : q|$return .= |) . $func . q|;
|;          }
# Function call (without spaces)
            elsif (my $func = $self->_check_func($tag, 1)) {
                $return .= ($indent x $indent_level) . ($print ? q|print | : q|$return .= |) . $func . q|;
|;          }
# Variable
            else {
                $return .= $indent x $indent_level;
                $return .= ($print ? q|print| : q|$return .=|) . q| $tmp if defined($tmp = $self->_get_var(q{| . _text_escape($tag) . q|}, { escape => $escape, strict => $strict }));
|;          }
        }
# 'if', 'ifnot', 'unless', 'elsif', 'elseif'
        elsif ($tag =~ s/^(if(?:not)?|unless|else?if)\b\s*//) {
            my $op = $1;
            $op = "unless" if $op eq "ifnot";
            $op = "elsif" if $op eq "elseif";
            if ($op eq 'elsif') {
                if ($i == -1 or $indent_level != $if_level[$i]) {
                    $return .= _comment($ERRORS->{UNMATCHEDELSIF});
                    $text->($ERRORS->{UNMATCHEDELSIF});
                    next;
                }
                elsif ($seen_else[$i]) {
                    $return .= _comment($ERRORS->{EXTRAELSIF});
                    $text->($ERRORS->{EXTRAELSIF});
                    next;
                }
                # for vim: {
                $return .= $indent x ($indent_level - 1) . q|}
|;              $return .= $indent x ($indent_level - 1) . q|elsif (|;
            }
            else {
                $seen_else[++$i] = 0;
                $return .= $indent x $indent_level++;
                $return .= "$op (";
                $if_level[$i] = $indent_level;
            }

            my @tests;
            my $bool = '';

            if ($tag =~ /\s(?:and|or)\s*(?:not)?\s/i) {
# Split the string into the individual expressions, but take care of quoted strings
                my @elements =
                    $tag =~/
                      ' (?:\\'|[^'])* '
                    | " (?:\\"|[^"])* "
                    | \s+(?:and|or)(?:\s*not)?\s+
                    | .
                    /xig;
                my $buf = '';
                for (@elements) {
                    if (/^\s+(and|or)(?:\s*(not))?\s$/i) {
                        push @tests, $buf if $buf;
                        $buf = '';

                        $bool = lc $1 eq 'and' ? ' and ' : ' or ' unless $bool;
                        push @tests, 'not' if $2;
                    }
                    else {
                        $buf .= $_;
                    }
                }
                push @tests, $buf if $buf;
            }
            else {
                @tests = $tag;
            }

            if ($tests[0] =~ s/^not\s+//) {
                unshift @tests, "not";
            }

            my @all_tests;
            my $one_neg;
            for my $tag (@tests) {
                if ($tag eq 'not') {
                    $one_neg = 1;
                    next;
                }
                my $this_neg = $one_neg ? $one_neg-- : 0;
                my $var;
                if ($tag =~ s{
                    ^
                    (\w+
                      (?:
                        (?:::\w+)+ # package::function(args) - (args) optional
                        (?:
                            \s* \(.+?\)
                        )?
                      |
                        \s* \(.+?\) # codevar(args) - (args) required
                      )
                    )
                    \s*}{}x) {
                    $var = $self->_check_func($1, 0);
                }
                elsif ($tag =~ s/^\$?([\w:.\$-]+)\b\s*//) {
                    $var = q|$self->_get_var(q{| . _text_escape($1) . q|}, { escape => 0, strict => 0, merge => 0 })|;
                }
                else {
                    next;
                }
                my ($comp, $casei, $val);
                if (length($tag)) {
                    if    ($tag =~ s/^(==?|!=|>=?|<=?|%|(i?)(?:eq|ne|g[et]|l[et]))\s*//) { $casei = $2 ? 1 : 0; $comp = " " . ($casei ? substr($1, 1) : $1) . " " }
                    elsif ($tag =~ s/^(i?)(?:like|contains)\s+//i)                       { $casei = $1 ? 1 : 0; $comp = "contains" }
                    elsif ($tag =~ s/^(i?)(start|end)s?\s+//i)                           { $casei = $1 ? 1 : 0; $comp = $2 }
                    $val = $tag if defined $comp;
                }
                $comp = ' == ' if $comp and $comp eq ' = ';
                my $full_comp = defined($comp);
                my $result = $this_neg ? 'not(' : '';
                if ($full_comp) {
                    if (substr($val,0,1) eq '$') {
                        substr($val,0,1) = '';
                        $val = q|$self->_get_var(q{| . _text_escape($val) . q|}, { escape => 0, strict => 0, merge => 0 })|;
                    }
                    elsif ($val =~ /^['"]/) {
                        $val = _quoted_string($val);
                    }
                    elsif (index($val, '::') > 0) {
                        $val = $self->_check_func($val, 0);
                    }
                    elsif ($val !~ /^[+-]?(?=\d|\.\d)\d*(?:\.\d*)?(?:[Ee](?:[+-]?\d+))?$/) {
                        $val = "q{" . _text_escape($val) . "}";
                    }
                    if ($casei) {
                        $val = "lc($val)";
                        $var = "lc($var)";
                    }
                    if ($comp eq 'contains') {
                        $result .= qq|index($var, $val) >= 0|;
                    }
                    elsif ($comp eq 'start') {
                        $result .= qq|substr($var, 0, length $val) eq $val|;
                    }
                    elsif ($comp eq 'end') {
                        $result .= qq|substr($var, -length $val) eq $val|;
                    }
                    elsif ($comp) {
                        $result .= qq|$var $comp $val|;
                    }
                }
                else { # Just a simple <%if var%> (Or something we don't understand, in which case we'll treat it like a simple <%if var%>)
                    $result .= $var;
                }
                $result .= ")" if $this_neg;
                push @all_tests, $result;
            }
            my $final_result = join $bool, @all_tests;
            $return .= $final_result;
            $return .= q|) {
|; # for vim: }
        }
# 'loop' - <%loop var%>, <%loop Pkg::Func(arg, $arg => arg)%>, <%loop var(arg, $arg => arg)%>, <%loop 1 .. $end%>, <%loop reverse whatever%>
        elsif ($tag =~ /^loop\s+(reverse\s+)?(.+)/s) {
            $loop_depth++;
            my $reverseloop = !!$1;
            my $loopon = $2;
            $return .= $self->_loop_on($loopon, $reverseloop, $indent, $indent_level, $loop_depth);
        }
# 'include $foo' - runtime includes based on variable value.
        elsif ($tag =~ /^include\s*\$(.*)/) {
            my $include_var = $1;
            $return .= $indent x $indent_level++;
            $return .= q|if (defined($tmp = $self->_get_var(q{| . _text_escape($include_var) . q|}, { escape => $escape }))) {
|;          $return .= $indent x $indent_level . ($print ? 'print ' : '$return .= ');
            $return .= q|$self->_include(ref $tmp eq 'SCALAR' ? $$tmp : $escape ? GT::CGI::html_escape($tmp) : $tmp);
|;          $return .= $indent x ($indent_level - 1) . q|}
|;          $return .= $indent x ($indent_level - 1) . q|else {
|;          $return .= $indent x $indent_level; # for vim: }
            $return .= ($print ? q|print q{| : q|$return .= q{|) . _text_escape(sprintf($ERRORS->{UNKNOWNINCLUDETAG}, $include_var)) . q|};
|;          $return .= $indent x --$indent_level . q|}
|;      }
# 'include' - load the file into the current template and continue parsing.
# The template must be added to this template's dependancy list.
# 'include $foo' is handled completely differently, above.
        elsif ($tag =~ /^include\b\s*([^\$].*)/) {
            my $include = $1;
            if ($self->{include_root} and $include =~ m{^(?:[a-zA-Z]:)?[/\\]}) {
                # Remove the drive letter on Windows
                $include =~ s/^[a-zA-Z]://;
                $include = $self->{include_root} . $include;
            }

            # If inside an if, but not a loop, turn this into a runtime include, so that:
            #   <%if foo%><%include bar.html%><%endif%>
            # is faster -- at least when foo is not set.  Compile-time includes are still
            # faster (as long as they are actually used) - but not by a significant amount
            # unless inside a largish loop.
            if (!$loop_depth and $i > -1 and not ($include eq '.' or $include eq '..' or $include =~ m{[/\\]})) {
                $return .= $indent x $indent_level;
                $return .= ($print ? 'print' : '$return .=') . q| $self->_include(q{| . _text_escape($include) . q|}, 1);
|;              next;
            }

            my $filename;
            if ($include =~ m{^(?:[a-zA-Z]:)?[/\\]}) {
                $filename = $include;
            }
            else {
                require GT::Template::Inheritance;
                $filename = GT::Template::Inheritance->get_path(path => $root, file => $include);
            }

            local *INCL;
            if ($filename and open INCL, "<$filename") {
                push @$files, [$include, $filename, (stat INCL)[9, 7]]; # mtime, size
                my $data = do { local $/; <INCL> };
                close INCL;
                substr($tpl, $last_pos - $tag_len, $tag_len) = $data . "$self->{begin}endinclude$self->{end}";
                $last_pos -= $tag_len;
                pos($tpl) = $last_pos;
                ++$include_safety <= GT::Template::INCLUDE_LIMIT or return $self->fatal('DEEPINC');

                $return .= $indent x $indent_level++ . q|{; | # The ; allows empty include files.     for vim: }
                    . _comment("Including $filename");

                push @include_ifdepth, [$i, $indent_level];
            }
            else {
                push @$files, [$include, $filename, -1, -1];
                my $errfile = $filename || "$root/$include";
                $return .= _comment(sprintf($ERRORS->{BADINC}, $errfile, "$!" || 'File does not exist'));
                $text->(sprintf($ERRORS->{BADINC}, $errfile, "$!" || 'File does not exist'));
            }
            next;
        }
# 'set' - set a value from the templates, optionally with a modifier (i.e. set
# foo = 4 vs. set foo += 4), also look for things like <%... x ...%>, <%... ~
# ...%>, etc., optionally with a 'set' on the front.  Filters are permitted as
# well.
#
#                            $1-3        $4, $5     $6           $7, $8     $9            $10           $11
        elsif ($tag =~ m{^(?:($RE_SET)(?:$RE_EXPR\s*($RE_MATH))?|$RE_EXPR\s*($RE_MATH))\s*($RE_FILTERS)?(.+)}os) {
            # $set is set if this is a 'set' (set foo = 3) as opposed to merely a modifier (foo + 3)
            # $setvar is the variable to set (obviously only if $set is set)
            # $change is set if this is a modifier assignment (i.e. 'set foo += 3' as opposed to 'set foo = 3')
            # $var is the value to set in a multi-value expression - i.e. bar in 'set foo = bar + 3', but undefined in 'set foo = $bar'
            #     or 'set foo = 3' - it can be a variable (i.e. without a $) or quoted string.
            # $var_filters are any filters that apply to $var, such as the 'escape_html' in 'set foo = escape_html $bar x 5'
            # $comp is the modifer to the value - such as the 'x' in 'set foo = $bar x 3'
            # $val is the actual value to set, and is the only parameter common to all cases handled here.  It can be a $variable,
            #     quoted string, or bareword string.
            # $val_filters are any filters to apply to $val
            my ($set, $setvar, $change, $var_filters, $var, $comp);
            my ($val_filters, $val) = ($10, $11);
            if ($1) {
                ($set, $setvar, $change, $var_filters, $var, $comp) = ($1, $2, $3 || '', $4, $5, $6);
            }
            else {
                ($var_filters, $var, $comp) = ($7, $8, $9);
            }

            if (defined $var) {
                if ($var =~ /^['"]/) {
                    $var = _quoted_string($var);
                }
                else {
                    substr($var,0,1) = '' if substr($var,0,1) eq '$';
                    $var = q|$self->_get_var(q{| . _text_escape($var) . q|}, { escape => $escape })|;
                }

                if ($var_filters) {
                    $return .= $indent x $indent_level;
                    $return .= "\$tmp2 = $var;\n";
                    $var = '$tmp2';
                    for (reverse split ' ', $var_filters) {
                        $return .= $indent x $indent_level;
                        $return .= _filter($_, '$tmp2') . "\n";
                    }
                }
            }

            my $func;
            if (substr($val,0,1) eq '$') {
                substr($val,0,1) = '';
                $val = q|$self->_get_var(q{| . _text_escape($val) . q|}, { escape => $escape |;

                if ($set and !defined $var and !$change and !$var_filters and !$comp and !$val_filters) {
                    $val .= ', return_ref => 1';
                }
                $val .= q| })|;
            }
            elsif ($val =~ /^['"]/) {
                $val = _quoted_string($val);
            }
            elsif (my $funccode = $self->_check_func($val, 0, 1)) {
                $val = '('. $funccode . ')';
                $func = 1;
            }
            else {
                $val = q|q{| . _text_escape($val) . q|}|;
            }
            if ($val_filters) {
                $return .= $indent x $indent_level;
                $return .= "\$tmp3 = $val;\n";
                $val = '$tmp3';
                for (reverse split ' ', $val_filters) {
                    $return .= $indent x $indent_level;
                    $return .= _filter($_, '$tmp3') . "\n";
                }
            }

            my $calc;
            if ($set and not defined $var) {
                $calc = $val;
            }
            else {
                $calc = _math($var, $comp, $val);
            }

            $return .= $indent x $indent_level;
            if ($set) {
                $return .= q|$tags->{q{| . _text_escape($setvar) . q|}} = do { my $none = (|;

                if ($change) {
                    # Passing $escape is required here, because what we save back
                    # is always a reference, thus the escaping has to occur here.
                    # $strict, however, is NOT passed because we aren't interested
                    # in variables becoming "Unknown tag: '....'"-type values.
                    $return .= _math(q|$self->_get_var(q{| . _text_escape($setvar) . q|}, { escape => $escape })|, $change, $calc);
                }
                else {
                    $return .= $calc;
                }
                $return .= '); ';
                if ($func) {
                    $return .= q[(ref $none and ref $none ne 'SCALAR' and ref $none ne 'LVALUE') ? $none : \$none];
                }
                else {
                    $return .= q[(ref $none eq 'ARRAY' or ref $none eq 'HASH') ? $none : \$none];
                }
                $return .= ' }';
            }
            else {
                $return .= ($print ? 'print ' : q|$return .= |) . $calc;
            }

            $return .= qq|;
|;      }
# Filters: 'escape_url', 'unescape_url', 'escape_html', 'unescape_html', 'escape_js', 'uc', 'ucfirst', 'lc', 'lcfirst', 'nbsp'
        elsif ($tag =~ /^($RE_FILTERS)(\S+)/o) {
            my $var = $2;
            my @filters = reverse split ' ', $1;

            $return .= $indent x $indent_level++;
            $return .= q|if (defined($tmp = $self->_get_var(q{| . _text_escape($var) . q|}, { escape => $escape, strict => $strict }))) {
|;          for (@filters) {
                $return .= $indent x $indent_level;
                $return .= _filter($_) . "\n";
            }
            $return .= $indent x $indent_level;
            $return .= ($print ? q|print| : q|$return .=|) . q| $tmp;
|;          $return .= $indent x --$indent_level . q|}
|;      }
# 'DUMP variable'
        elsif ($tag =~ /^DUMP\s+\$?(\w+(?:\.\$?\w+)*)$/) {
            my $func = qq{\$self->_call_func('GT::Template::dump', \$strict, 0, -auto => 1, -var => '$1')};
            $return .= ($indent x $indent_level) . ($print ? q|print | : q|$return .= |) . $func . q|;
|;      }
# 'init array variable' and 'init hash variable'
        elsif ($tag =~ /^init\s+(array|hash)\s+\$?(\w+(?:\.\$?\w+)*)$/i) {
            $return .= q|$tags->{q{| . _text_escape($2) . q|}} = | . (lc $1 eq 'array' ? '[]' : '{}') . q|;
|;      }
        elsif (my $func = $self->_check_func($tag, 1)) {
            $return .= ($indent x $indent_level) . ($print ? q|print | : q|$return .= |) . $func . q|;
|;      }
        else {
            # Check to see if it's a valid variable, function call, etc.  Force
            # strict on because this is some sort of strange tag that doesn't
            # appear to be a variable, which should always produce an "Unknown
            # tag" warning.
            $return .= $indent x $indent_level;
            $return .= ($print ? q|print| : q|$return .=|) . q| $tmp if defined($tmp = $self->_get_var(q{| . _text_escape($tag) . q|}, { escape => $escape, strict => 1 }));
|;      }
    }
    $text->(substr($tpl, $last_pos));
    while ($indent_level > 0) {
        $return .= ($indent x --$indent_level) . q|}
|   }
    $return .= $print ? q|return 1;| : q|return \$return;|;
    return \$return;
}

# Handles quoted string semantics.
#
# Inside double-quote strings:
# \ can preceed any non-word character to mean the character itself - following
# word characters the following escapes are currently supported: \n, \r, \t,
# \000 (octal character value), \x00 (hex character value).  \ followed by any
# other word character is undefined behaviour and should not be used.
# Variables are interpolated - you can write a variable as $foo.bar or
# ${foo.bar}.  Inner-variable interpolation (such as what happens in
# <%foo.$bar%> is supported only in the latter form: ${foo.$bar} - $foo.$bar
# would end up becoming the value of foo, a ., then the value of bar.
#
# Inside single-quote strings:
# \ can preceed \ or ' to mean the value; preceeding anything else a \ is a
# literal \
%ESCAPE_MAP = (
    t => '\t',
    n => '\n',
    r => '\r',
);
sub _quoted_string {
    my $string = shift;
    if ($string =~ s/^"//) {
        $string =~ s/"$//;
        $string =~ s[
            (\\) # $1 A backslash escape of some sort
            (?:
                (x[0-9a-fA-F]{2}) # $2 - \x5b - a hex char
            |
                ([0-7]{1,3}) # $3 - \123 - an octal char
            |
                (\w) # $4 - a word char - \n, \t, etc.
            |
                (\W) # $5 - a non word char - \\, \", etc.
            )
        |
            \$ # The dollar sign that starts a variable
            (?:
                { # opening { in a ${var}-style variable  ## vim: }
                    (\w+(?:\.\$?\w+)*) # $6 - the inner part of a ${var} variable
                }
            |
                (\w+) # $7 - the name of a $var-style variable
            )
        |
            ([{}\\]) # $8 - a character that needs to be escaped inside the q{}-delimited string - the \\ will only
                     # match at the very end of the string - though "string\" isn't really valid.
        ][
            if ($1) { # a \ escape
                if (my $code = $2 || $3) {
                    qq|}."\\$code".q{|;
                }
                elsif (defined $4 and exists $ESCAPE_MAP{$4}) {
                    qq|}."$ESCAPE_MAP{$4}".q{|;
                }
                elsif (defined $4) {
                    qq|}."$4".q{|;
                }
                else {
                    qq|}."\\$5".q{|;
                }
            }
            elsif ($8) {
                "\\$8"
            }
            else { # A variable
                my $variable = $6 || $7;
                q|}.$self->_get_var(q{| . _text_escape($variable) . q|}, { escape => 1, strict => 1 }).q{|;
            }
        ]egsx;
    }
    elsif ($string =~ s/^'//) {
        $string =~ s/'$//;
        $string =~ s/\\(['\\])/$1/g;
        $string = _text_escape($string);
    }
    "q{$string}";
}
sub _math {
    my ($left, $comp, $right) = @_; # var => left, val => right
    my $calc;
    if    ($comp =~ /^[.*+-]$/ or $comp eq '||' or $comp eq '&&') { $calc = "+(($left) $comp ($right))" }
    elsif ($comp =~ m{^/(\d+)$}) { $calc = "+sprintf(q{%.$1f}, (((\$tmp = ($right)) != 0) ? (($left) / \$tmp) : 0))" }
    elsif ($comp eq '/')         { $calc = "+(((\$tmp = ($right)) != 0) ? ($left / \$tmp) : 0)" }
    elsif ($comp eq 'i/')        { $calc = "int(((\$tmp = ($right)) != 0) ? (int($left) / int(\$tmp)) : 0)" }
    elsif ($comp eq '%')         { $calc = "+(((\$tmp = ($right)) != 0) ? ($left % \$tmp) : 0)" }
    elsif ($comp eq '~')         { $calc = "+(((\$tmp = ($right)) != 0) ? (\$tmp - ($left % \$tmp)) : 1)" }
    elsif ($comp eq '^')         { $calc = "+(($left) ** ($right))" }
    elsif ($comp eq 'x')         { $calc = "+(scalar($left) x ($right))" }
    $calc ||= '';
    $calc;
}

sub _loop_on {
    my ($self, $on, $reverse, $indent, $indent_level, $loop_depth) = @_;
    $reverse = $reverse ? 1 : 0;

    my $var;

    if ($on =~ /^(\d+|\$[\w.\$-]+)\s+(?:\.\.|to)\s+(\d+|\$[\w.\$-]+)$/) {
        my ($start, $end) = ($1, $2);
        for ($start, $end) {
            $_ = q|int(do { my $v = $self->_get_var(q{| . _text_escape($_) . q|}); ref $v ? 0 : $v })|
                if s/^\$//;
        }
        $var = "[$start .. $end]";
    }
    elsif (index($on, '::') > 0 or index($on, '(') > 0) {
        $var = $self->_check_func($on, 0);
    }
    else {
        $on =~ s/^\$//;
        $var = q|$self->_raw_value(q{| . _text_escape($on) . q|})|;
    }

    my $print = $self->{opt}->{print};
    my $i0 = $indent x $indent_level;
    my $i = $indent x ($indent_level + 1);
    my $i____ = $indent x ($indent_level + 2);
    my $i________ = $indent x ($indent_level + 3);
    my $i____________ = $indent x ($indent_level + 4);
    my $i________________ = $indent x ($indent_level + 5);
    my $return = <<CODE;
${i0}\{
${i}my \$orig = {\%{\$self->{VARS}}};
${i}my %loop_set;
${i}LOOP$loop_depth: \{
${i____}my \$loop_var = $var;
${i____}my \$loop_type = ref \$loop_var;
${i____}if (\$loop_type eq 'CODE' or \$loop_type eq 'ARRAY') {
${i________}my \$next;
${i________}my \$row_num = 0;
${i________}my \$i = ($reverse and \$loop_type eq 'ARRAY') ? \$#\$loop_var : 0;
${i________}my \$current = \$loop_type eq 'CODE' ? \$loop_var->() : \$loop_var->[$reverse ? \$i-- : \$i++];
${i________}if (\$loop_type eq 'CODE' and ref \$current eq 'ARRAY') {
${i____________}\$loop_type = 'ARRAY';
${i____________}\$loop_var = \$current;
${i____________}\$i = $reverse ? \$#\$loop_var : 0;
${i____________}\$current = \$loop_var->[$reverse ? \$i-- : \$i++];
${i________}}
${i________}while (defined \$current) {
${i____________}if (\$loop_type eq 'CODE') {
${i________________}\$next = \$loop_var->();
${i____________}}
${i____________}else {
${i________________}\$next = ($reverse and \$i < 0) ? undef : \$loop_var->[$reverse ? \$i-- : \$i++];
${i____________}}
${i____________}my \$copy = {\%{\$self->{VARS}}};
${i____________}for (keys %loop_set) {
${i________________}\$copy->{\$_} = \$orig->{\$_};
${i________________}delete \$loop_set{\$_};
${i____________}}
${i____________}for (qw/rownum row_num first last inner even odd loop_value/, ref \$current eq 'HASH' ? (keys \%\$current) : ()) { \$loop_set{\$_} = 1 }
${i____________}\$copy->{row_num} = \$copy->{rownum} = ++\$row_num;
${i____________}\$copy->{first}   = (\$row_num == 1) || 0;
${i____________}\$copy->{last}    = (not defined \$next) || 0;
${i____________}\$copy->{inner}   = (!\$copy->{first} and !\$copy->{last}) || 0;
${i____________}\$copy->{even}    = (\$row_num % 2 == 0) || 0;
${i____________}\$copy->{odd}     = (not \$copy->{even}) || 0;
${i____________}if (ref \$current ne 'HASH') { \$current = { loop_value => \$current } }
${i____________}else { \$loop_set{loop_value} = 1; \$copy->{loop_value} = \$current }
${i____________}for (keys \%\$current) { \$copy->{\$_} = \$current->{\$_} }
${i____________}\$self->{VARS} = \$copy;
${i____________}\$current = \$next;

CODE
    $_[4] += 4; # Update the indent level
    return $return;
}



sub _check_func {
# ---------------------------------------------------------------
# Takes a string and if it looks like a function, returns a string that will
# call the function with the appropriate arguments.  Takes a second argument
# which, if true, will pass the strict argument to _call_func, and a third to
# indicate that this is inside a set (and therefore, to not pollute the
# variable environment if a hash is returned).  strict mode should only be
# enabled for straight function calls as otherwise <%if Function::foo%> will
# return a "true" error message and thus succeed.
#
# So, you enter the tag (without the <% and %>):
#   <%GFoo::function($foo, $bar, $boo, $far, '7', 'text')%>
# and you'll get back:


#   $self->_call_func('GFoo::function', 1, 0, $self->_get_var(q{foo}, { escape => 0, strict => 0 }), $self->_get_var(q{bar}, { escape => 0, strict => 0 }), ..., q{7}, q{text});


#   <%codevar($foo, $bar, $boo, $far => 7, text)%>
#   $self->_call_func('codevar', 1, 0, $self->_get_var(q{foo}, { escape => 0, strict => 0 }), $self->_get_var(q{bar}, { escape => 0, strict => 0 }), ..., q{7}, q{text});


# NOTE: NO SEMICOLON (;) ON THE END
# which will require GFoo and call GFoo::function with the arguments provided.
#
# If you call this with a tag that doesn't look like a function, undef is returned.
#
    my ($self, $str, $strict, $set) = @_;
    my $ret;
    if (((index($str, '(') >= 0 and rindex($str, ')') >= 0) or index($str, '::') >= 1) and $str =~ /^
        (?:
# Package $1
            (
                \w+
                (?:
                    ::
                    \w+
                )*
            )
            ::
        )?
# Function $2
        (
            \w+
        )
        \s*
# Any possible arguments
        (?:
            \(
            \s*
            (
                .+? # Arguments list $3
            )?
            \s*
            \)
        )?
    $/sx) {
        my ($package, $func, $args) = ($1, $2, $3);
        $ret = '';
        $args = '' if not defined $args;

        $args = join ", ", _parse_args($args) if length $args;

        $ret = q|$self->_call_func('| . ($package ? "$package\::$func" : $func) . q|', | . ($strict ? 1 : 0) . q|, | . ($set ? 1 : 0);
        $ret .= ", $args" if $args;
        $ret .= ")";
    }
    return $ret;
}

sub _parse_args {
# --------------------------------------------------------
# Splits up arguments on commas outside of quotes. Unquotes
#
    my $line = shift;
    my ($word, @pieces);
    local $^W;
    while (length $line) {
        my ($quoted, undef, $bareword, $delim) = $line =~ m{
            ^
            (?:
                (                           # $quoted test
                    (["'])                  # the actual quote
                    (?:\\.|(?!\2)[^\\])*    # the text
                    \2                      # followed by the same quote
                )
            |                               # --OR--
                ((?:\\.|[^\\"'])*?)         # $bareword text, plus:
                (                           # $delim
                    \Z(?!\n)                # EOL
                |
                    \s*(?:,|=>)\s*          # delimiter
                |
                    (?!^)(?=["'])           # or quote
                )
            )
            (.*)                            # and the rest ($+)
        }sx;
        return unless $quoted or length $bareword or length $delim;

        $line = $+;

        my $val;
        if ($quoted) {
            $val = _quoted_string($quoted);
        }
        elsif ($bareword =~ s/^\$//) {
            $val = q|$tags->{q{| . _text_escape($bareword) . q|}}|;
        }
        elsif (length $bareword) {
            $bareword =~ s/\\(.)/$1/g;
            $val = q|q{| . _text_escape($bareword) . q|}|;
        }

        $word = $word ? "$word.$val" : $val if defined $val;

        if (length $delim) {
            push @pieces, $word;
            $word = undef;
        }
    }
    push @pieces, $word if defined $word;
    return @pieces;
}

1;


} # End of BEGIN for GT/Template/Parser.pm

BEGIN {
    $INC{"GT/Template/Vars.pm"} = "GT/Template/Vars.pm";

# ====================================================================
# Gossamer Threads Module Library - http://gossamer-threads.com/
#
#   GT::Template::Vars
#   Author: Jason Rhinelander
#   $Id: Vars.pm,v 1.8 2006/12/06 23:55:52 brewt Exp $
#
# Copyright (c) 2005 Gossamer Threads Inc.  All Rights Reserved.
# ====================================================================
#
# Description:
#   GT::Template variable handling tied hash reference.
#

package GT::Template::Vars;
use strict;
use Carp 'croak';

sub TIEHASH {
    my ($class, $tpl) = @_;

    my $self = { t => $tpl, keys => [] };
    bless $self, ref $class || $class;
}

sub STORE {
    my ($self, $key, $value) = @_;
    if ($key =~ /^\w+(?:\.\$?\w+)+$/) {
        my $cur = \$self->{t}->{VARS};
        my @set = split /\./, $key;
        for (my $i = 0; $i < @set; $i++) {
            if ($set[$i] =~ /^\$/) {
                my $val = $self->{t}->_get_var(substr($set[$i], 1));
                $val = '' if not defined $val;
                my @pieces = split /\./, $val;
                @pieces = '' if !@pieces;
                splice @set, $i, 1, @pieces;
                $i += @pieces - 1 if @pieces > 1;
            }
        }
        while (@set) {
            my $k = shift @set;
            if ($k =~ s/^\$//) {
                $k = '' . ($self->FETCH($k) || '');
            }
            if (ref $$cur eq 'ARRAY' and $k =~ /^\d+$/) {
                $cur = \$$cur->[$k];
            }
            elsif (ref $$cur eq 'ARRAY' and $k eq 'push') {
                $cur = \$$cur->[@$$cur];
            }
            elsif (ref $$cur eq 'ARRAY' and $k =~ /^last(\d+)?$/) {
                $cur = \$$cur->[-($1 || 1)];
            }
            elsif (ref $$cur eq 'HASH' or not defined $$cur or UNIVERSAL::isa($$cur, 'GT::Config')) {
                if (exists $$cur->{$k} and ref $$cur->{$k} eq 'SCALAR') {
                    $set[0] = $k . '.' . $set[0];
                }
                else {
                    $cur = \$$cur->{$k};
                }
            }
            elsif (UNIVERSAL::isa($$cur, 'GT::CGI') and !@set) {
                # You can set a GT::CGI parameter, but only to a scalar value (or reference to a scalar value)
                return $$cur->param(
                    $k => ((ref $value eq 'SCALAR' or ref $value eq 'LVALUE') and not ref $$value) ? $$value : "$value"
                );
            }
            else {
                croak 'Not a HASH reference';
            }
        }
        $$cur = $value;
    }
    else {
        $self->{t}->{VARS}->{$key} = $value;
    }
}

# Fetching returns the template parser's raw value, bypassing the usual
# _get_var-based approach which can escape, be strict, and will flatten
# references.
sub FETCH {
    my ($self, $key) = @_;
    my $value = $self->{t}->_raw_value($key);
    $value = $$value if ref $value eq 'SCALAR' or ref $value eq 'LVALUE';
    return $value;
}

# Keys/exists are a little strange - if "foo" is set to { a => 1 }, exists
# $tags->{"foo.a"} will be true, but only "foo", not "foo.a", will be returned
# by keys %$tags.
sub FIRSTKEY {
    my $self = shift;
    my @keys;
    for (keys %{$self->{t}->{VARS}}) {
        push @keys, $_;
    }
    for (keys %{$self->{t}->{ALIAS}}) {
        push @keys, $_ unless exists $self->{t}->{VARS}->{$_};
    }

    $self->{keys} = \@keys;

    return shift @keys;
}

sub EXISTS {
    my ($self, $key) = @_;
    my @val = $self->{t}->_raw_value($key);
    return !!@val;
}

sub NEXTKEY {
    my $self = shift;
    if (!$self->{keys}) {
        return $self->FIRSTKEY;
    }
    elsif (!@{$self->{keys}}) {
        delete $self->{keys};
        return;
    }
    return shift @{$self->{keys}};
}

sub DELETE {
    my ($self, $key) = @_;
    my $value = $self->FETCH($key);
    delete $self->{t}->{VARS}->{$key};
    $value;
}
sub CLEAR  { %{$_[0]->{t}->{VARS}} = () }
sub SCALAR { scalar %{$_[0]->{t}->{VARS}} }

1;


} # End of BEGIN for GT/Template/Vars.pm



%GT::Installer::LANG = (
    ERR_REQUIRED   => "%s cannot be left blank.",
    ERR_PATH       => "The path (%s) does not exist on this system",
    ERR_PATHWRITE  => "Unable to write to directory '%s': %s",
    ERR_PATHCREATE => "Unable to create directory '%s': %s",
    ERR_URLFMT     => "'%s' is not a valid URL",
    ERR_FTPFMT     => "'%s' is not a valid FTP URL",
    ERR_EMAILFMT   => "'%s' is not a valid e-mail address",
    ERR_SENDMAIL   => "The sendmail path (%s) does not exist on your system or is not executable",
    ERR_SMTP       => "'%s' is not a valid SMTP server address",
    ERR_PERL       => "The path to perl you specified (%s) %s",
    ERR_DIREXISTS  => "Directory '%s' cannot be created; a file/directory with the same name already exists",
    ERR_WRITEOPEN  => "Could not write to '%s': %s\n",
    ERR_READOPEN   => "Could not read '%s': %s\n",
    ERR_RENAME     => "Could not rename '%s' to '%s': %s\n",
    ENTER_REG      => 'Please enter your registration number',
    REG_NUM        => 'Registration Number',
    ENTER_SENDMAIL => 'Please enter either a path to sendmail, or an SMTP server to use for sending mail',
    MAILER         => 'Mailer',
    ENTER_PERL     => 'Please enter the path to perl',
    PATH_PERL      => 'Path to perl',
    CREATE_DIRS    => 'Create Directories',
    INSTALL_CURRUPTED => '
install.dat appears to be corrupted.  If you are using FTP to transer the file
be sure to upload the file in BINARY mode.

If you need assistance, please visit:
    http://www.gossamer-threads.com/scripts/support/
',
   INSTALL_VERSION => '
This program requires Perl version 5.004_04 or greater to run.  Your
system is only running version %s.  Try changing the path to perl in
install.cgi to a newer version, or contact your ISP for help.
',
   ADMIN_PATH_ERROR => "You must specify the path to the previous installation's admin area",
   INTRO => '
%s Quick Install http://www.gossamer-threads.com
Copyright (c) 2008 Gossamer Threads Inc.  All Rights Reserved
Redistribution in part or in whole strictly prohibited.
',
    WELCOME => '
Welcome to %s.  This program will unarchive and install %s.

To begin, please enter the following information.  Type exit
or quit at any time to abort.
',
    IS_UPGRADE        => "Is this an upgrade of an existing installation",
    ENTER_ADMIN_PATH  => "\nPlease enter path to current admin",
    UNARCHIVING       => 'Unarchiving',
    TAR_OPEN          => "Could not open '%s': %s",
    TAR_READ          => "There was an error reading from '%s': expected to read %s bytes, but only got %s.",
    TAR_BINMODE       => "Could not binmode '%s': %s",
    TAR_BADARGS       => "Bad arguments passed to %s: %s",
    TAR_CHECKSUM      => "Checksum Error parsing tar file.  Most likely this is a corrupt tar.\nHeader: %s\nChecksum: %s\nFile: %s\n",
    TAR_NOBODY        => "File '%s' does not have a body!",
    TAR_CANTFIND      => "Unable to find a file named: '%s' in tar archive.",
    TAR_CHMOD         => "Could not chmod %s, Reason: %s",
    TAR_DIRFILE       => "Directory '%s' cannot be created; a file with the same name already exists",
    TAR_MKDIR         => "Could not mkdir %s, Reason: %s",
    TAR_RENAME        => "Unable to rename temp file: '%s' to tar file '%s'. Reason: %s",
    TAR_NOGZIP        => "Compress::Zlib module is required to work with .tar.gz files.",
    SKIPPING_FILE     => "Skipping %s\n",
    OVERWRITTING_FILE => "Overwriting %s\n",
    SKIPPING_MATCHED  => "Skipping %s in matched directory\n",
    BACKING_UP_FILE   => "Backing up %s\n",
    ERR_OPENTAR => '
Unable to open the install.dat file! Please make sure the
file exists and that the permissions are set properly so the
program can read the file.

The error message was:
    %s

If you need assistance, please visit:
    http://www.gossamer-threads.com/scripts/support/
    ',
    ERR_OPENTAR_UNKNOWN => '
Unknown error opening tar file:
    %s

If you need assistance, please visit:
http://www.gossamer-threads.com/scripts/support/
',
    WE_HAVE_IT => "\nWe have everything we need to proceed.\n\n",
    ENTER_STARTS => "\nPress ENTER to install, or CTRL-C to abort",
    NOW_UNARCHIVING => '

We are now unarchiving %s and will be extracting
all the files shortly.  Please be patient...
',
    UPGRADE_DONE => '

Congratulations! Your copy of %s has now been 
updated to version %s.  The install files have 
been removed.

If you need to re-run the install, please unarchive the 
original file again.
',
    INSTALL_DONE => '

%s is now unarchived.  The install files have been 
removed.  If you need to re-run the install, please unarchive 
the original file again.

NOTE: Please do not leave your original .tar.gz file in your
web directory!

',
    TELNET_ERR => 'Error: %s',
    INSTALLER_CSS => <<'CSS', #> - help vim in ft=html mode
<style type="text/css">
/*<![CDATA[*/
body {
  margin: 0px;
  padding: 0px;
  color: #33332e;
  background: #ffffff;
  font: normal 11px tahoma, geneva, verdana, sans-serif;
  text-align: center;
}

/* forms */
form {
  margin: 0px;
}
input, textarea {
  font: normal 13px tahoma, geneva, verdana, sans-serif;
}
input.text, input.password, textarea {
  padding: 1px 0px 1px 2px;
  border: 1px solid #57594b;
}

/* links */
a {
  color: #212126;
}
a:visited {
  color: #8e8d9a;
}
a:hover {
  color: #5c5b66;
  text-decoration: none;
}

/* headings */
h1 {
  font-size: 16px;
}
h2 {
  font-size: 15px;
}
h3 {
  font-size: 13px;
}
h4 {
  font-size: 12px;
}
h5 {
  font-weight: normal;
  font-size: 11px;
}

h2.error {
  margin-top: 0px;
  color: red;
}
  
.no-bmargin {
  margin-bottom: 0px;
}

/*--------*\
|* header *|
\*--------*/
#logo {
  width: 250px;
  height: 80px;
  background: transparent url(http://www.gossamer-threads.com/images/installer/<%escape_url product%>%20logo.gif) top left no-repeat;
  text-indent: -9999px;
}
#logo h1, #logo a {
  margin: 0px;
  width: 100%;
  height: 100%;
}
#logo a {
  text-decoration: none;
  display: block;
}
#header {
  background: transparent url(http://www.gossamer-threads.com/images/installer/swirls.gif) top right no-repeat;
}

.shadowtop, .shadowbottom {
  height: 10px;
  font-size: 1px;
  display: inline-block;
/* ie/mac fix \*/
  display: block;
/* end fix */
}
.shadowtopleft, .shadowtopright, .shadowbottomleft, .shadowbottomright {
  width: 15px;
  height: 10px;
  font-size: 1px;
}
.shadowtop {
  background: url(http://www.gossamer-threads.com/images/installer/shadow-top.gif) bottom repeat-x;
}
.shadowbottom {
  background: url(http://www.gossamer-threads.com/images/installer/shadow-bottom.gif) top left repeat-x;
}
.shadowleft {
  background: url(http://www.gossamer-threads.com/images/installer/shadow-left.gif) left repeat-y;
}
.shadowright {
  background: url(http://www.gossamer-threads.com/images/installer/shadow-right.gif) right repeat-y;
}
.shadowtopleft {
  float: left;
  background: url(http://www.gossamer-threads.com/images/installer/shadow-topleft.gif) bottom left no-repeat;
}
.shadowtopright {
  float: right;
  background: url(http://www.gossamer-threads.com/images/installer/shadow-topright.gif) bottom right no-repeat;
}
.shadowbottomleft {
  float: left;
  background: url(http://www.gossamer-threads.com/images/installer/shadow-bottomleft.gif) top left no-repeat;
}
.shadowbottomright {
  float: right;
  background: url(http://www.gossamer-threads.com/images/installer/shadow-bottomright.gif) top right no-repeat;
}

/* form */
.row {
  margin-bottom: 1px;
  padding: 4px;
}
.row label.name {
  padding-right: 5px;
  width: 25%;
  float: left;
}
.row label.name span {
  color: #8c3030;
}
.row .value.wrappedtext {
  margin-left: 25%;
  padding-left: 5px;
}
/* hack for ie 5.5 text/textarea resizing */
.row .value input.text, .row .value input.password, .row .value textarea {
  width: expression(this.parentNode.offsetWidth * 0.74);
}
/* the previous style causes problems with long sidebars in ie6 */
.row .value input.text, .row .value input.password, .row .value textarea {
  w\idth: 72%;
}
.row .value input.text.shorttext {
  width: 100px;
}
.row.required {
  background: #e2e1eb;
}
.row.invalid {
  color: #8c3030;
  background: #eedddd;
  font-weight: bold;
}
.formsubmit {
  margin-top: 10px;
  text-align: right;
}

.wrapper {
  margin: 0px auto;
  width: 750px;
  text-align: left;
}

.content {
  margin: 0px 10px;
  padding: 10px;
  background: #ffffff;
}

.hide {
  display: none;
}
.clear:after {
  height: 0px;
  clear: both;
  display: block;
  visibility: hidden;
  content: ".";
}
.clear {
  display: inline-block;
}
/* start non ie/mac css \*/
* html .clear {
  height: 1%;
}
.clear {
  display: block;
}
/* end non ie/mac css */

.bar1 {
    height: 10px;
  background-color: #5c5b66;
  margin: 0px 0px 2px 0px;
}

.bar2 {
    height: 10px;
  background-color: #edf2cb;
  border-bottom: 1px solid #57394b;
  margin: 0px 0px 3px 0px;
}

/*---------------*\
|* contentheader *|
\*---------------*/
#contentheader {
  padding: 10px 10px 0px 10px;
  background: #e2e1eb;
}
#contentheader .error, #contentheader .message {
  margin: 0px 50px;
  padding: 2px;
  border: 1px solid #ffffff;
  color: #ffffff;
  font-size: 12px;
  text-align: center;
}
#contentheader .error ul, #contentheader .message ul {
  list-style: none;
}
#contentheader .error {
  background: #8c3030;
}
#contentheader .message {
  background: #3a5841;
}
#contentheader .error *, #contentheader .message * {
  margin: 0px;
  padding: 0px;
}

blockquote {
  text-align: left;
}

.complete-installation {
  font-size: 14px;
  font-weight: bold;
}

/*--------*\
|* footer *|
\*--------*/
#footer {
  margin-top: 3px;
  padding: 5px;
  border-top: 1px solid #57594b;
  background: #edf2cb;
}
#footer a {
  float: right;
}
#footer p {
  margin: 0px;
  padding: 10px 0px;
  color: #8a8d77;
  font-size: 9px;
}
#powered_by {
  display: table-cell;
  display: inline-block;
  width: 100px;
  height: 33px;
  background: url(http://www.gossamer-threads.com/images/installer/<%escape_url product%>.gif) no-repeat right;
}
/*]]>*/
</style>
CSS
    TELNET_EULA => <<'EULA',
GOSSAMER THREADS INC.
END USER PRODUCT LICENSE AGREEMENT

IMPORTANT:	READ THIS LICENSE BEFORE INSTALLING THE SOFTWARE

This software product (the "Software") and the accompanying
documentation (the "Documentation") (together, the "Product") are
provided only under license from Gossamer Threads Inc. ("GTI") to its
customers for their use only as set forth in this Agreement. You should
carefully read the following terms and conditions before downloading,
installing and using the Software or using the Documentation. Installing
or otherwise using any part of the Software indicates that you accept
these terms and conditions. If you do not agree with the terms and
conditions of this Agreement, do not download, install or otherwise use
the Software and do not click on the "I agree" or similar button. If you
have received the Product on physical media, return the entire product
with the software and documentation unused to the supplier where you
obtained it.

1.      License:  GTI grants you a personal, non exclusive, license to use the
        Software in executable form and the Documentation, subject to the terms
        and restrictions set forth in this Agreement.  You are not permitted to
        lease, rent, distribute, publish or sub license the Software or the
        Documentation or to use any part of the Product in a time sharing
        arrangement or in any other unauthorized manner provided that you may
        transfer all your rights in the Product to another person as long as
        you remove all copies from your computers and cease all use of it.  GTI
        does not grant you any license in the source code of the Software.
        This Agreement does not grant you any rights to patents, copyrights,
        trade secrets, trademarks or any other rights with respect to the
        Product.
<%if product eq 'Gossamer Mail' and not version ends '-Single'%>
2.      Permitted Use.  You are authorized to have, at any time while this
        license is valid, one installation of the Software only, and you may
        remove the Software from one server and install it on another as long
    <%~if version ends '-Enterprise' or version ends '-Upgrade'%>
        as it is running on only one server at any time.
    <%~elsif version ends '-Professional'%>
        as it is running on only one server for a maximum of one hundred
        domains only at any time.
    <%~else%>
        as it is running on only one server for a maximum of five domains only
        at any time.
    <%~endif%>
<%else%>
2.      Permitted Use.  You are authorized to have, at any time while this
        license is valid, one installation of the Software for one domain
        only, and you may remove the Software from one server and install it
        on another as long as it is running on only one server for one domain
        only at any time.
<%endif%>
3.      Backup and Copyright Notices.  You may reproduce one copy of the
        Software and the Documentation for backup or archive purposes.  Any
        such copies must contain GTI's and its licensors' proprietary rights
        and copyright notices in the same form as on the original.  You agree
        not to remove or deface any portion of any legend provided on any
        licensed program or documentation delivered to you under this
        Agreement.

4.      Modification.  You may make unlimited modifications to the Software
        for your own internal use only, but any support obligations of GTI
        with respect to the Software will be terminated if you do so.

5.      LIABILITY LIMITATIONS.  IN NO EVENT WILL GTI BE LIABLE TO YOU OR TO
        ANY OTHER PARTY FOR DIRECT, INDIRECT, SPECIAL, INCIDENTAL OR
        CONSEQUENTIAL DAMAGES ARISING OUT OF THE USE OF THE SOFTWARE, THE
        DOCUMENTATION OR ANY DERIVATIVES THEREOF, EVEN IF GTI HAS BEEN ADVISED
        OF THE POSSIBILITY OF SUCH DAMAGE.  GTI SPECIFICALLY DISCLAIMS ANY
        WARRANTIES, INCLUDING BUT NOT LIMITED TO ANY IMPLIED WARRANTIES OF
        MERCHANTABILITY, FITNESS FOR A PARTICULAR PURPOSE AND
        NON-INFRINGEMENT.  THE SOFTWARE AND THE DOCUMENTATION ARE PROVIDED ON
        AN "AS IS" BASIS, AND GTI HAS NO OBLIGATION TO PROVIDE MAINTENANCE,
        SUPPORT, UPDATES, ENHANCEMENTS OR MODIFICATIONS EXCEPT AS SPECIFICALLY
        AGREED UPON.

6.      Ownership.  You acknowledge and agree that the structure, sequence and
        organization of the Software are valuable trade secrets of GTI and
        that you will hold such trade secrets in confidence.  You further
        acknowledge and agree that ownership of and title to the Product and
        all subsequent copies thereof, regardless of the form or media, are
        held by GTI.

7.      Indemnity.  You will indemnify and save GTI harmless from any and all
        actions, damages, liabilities, charges, claims and associated expenses
        ("Claims") against or incurred by GTI in any way connected with your
        use of the Product, whether such Claims are by you or by any third
        parties as a result of or related to your use of the Software.

8.      Termination.  The licenses granted hereunder are perpetual unless
        terminated earlier as specified below.  You may terminate the licenses
        and this Agreement at any time by destroying the Software and the
        Documentation together with all copies and merged portions in any
        form.  The licenses and this Agreement will also terminate immediately
        and automatically without notice if you fail to comply with any term
        or condition of this Agreement.  Upon such termination you agree to
        destroy the Software and the Documentation, together with all copies
        and merged portions in any form.  GTI will not be liable for any
        refund to you on termination of this Agreement for any reason.

9.      Government Use.  If you are acquiring the Software on behalf of the
        U.S. government, the Government shall have only "Restricted Rights" in
        the Software and the Documentation as defined in clause 52.227
        19(c)(2) of the U.S. Federal Acquisition Regulations.

10.     Severability.  If any provision of this Agreement is found to be
        invalid, illegal or unenforceable, the validity, legality and
        enforceability of any of the remaining provisions shall not in any way
        be affected or impaired and a valid, legal and enforceable provision
        of similar intent and economic impact shall be substituted therefor.

11.     Entire Agreement: This Agreement sets forth the entire understanding
        and agreement between you and GTI and supersedes all prior agreements
        with respect to the Product.  GTI may change the terms of this
        Agreement by electronic notice to you.

12.     Governing Law.  This Agreement shall be governed by the laws of the
        Province of British Columbia and the federal laws of Canada applicable
        therein excluding its conflicts of laws principles and excluding the
        United Nations Convention on Contracts for the International Sale of
        Goods.

EULA
    EULA_PROMPT => 'Do you accept the terms of the above license agreement?',
    EULA_REQUIRED => 'You must accept the terms of the license agreement before
proceeding with the installation.

',
    HTML_EULA => <<'EULA',
<!DOCTYPE html PUBLIC "-//W3C//DTD XHTML 1.0 Transitional//EN" "http://www.w3.org/TR/xhtml1/DTD/xhtml1-transitional.dtd">
<html>
  <head>
    <title><%product%> <%version%> - Installation - End User Product License Agreement</title>
    <%css%>
    <style type="text/css">
/*<![CDATA[*/
      .eula ol {
        list-style-type: decimal;
      }
/*]]>*/
    </style>
  </head>
  <body>
    <div class="wrapper">
      <div id="header">
        <div id="logo"><h1><a href="http://www.gossamer-threads.com"><%product%></a></h1></div>
      </div>

      <hr class="hide" />
      <div class="bar1"></div>
      <div class="bar2"></div>

      <%if in.eula_displayed~%>
      <div id="contentheader">
        <div class="error"><%GT::Installer::tpllang('eula_required')%></div>
      </div>
      <%~endif%>

      <div class="shadowleft">
        <div class="shadowtop"><div class="shadowtopleft"></div><div class="shadowtopright"></div></div>
        <div class="shadowright">
          <div class="content eula">

            <div class="crumb">
              <span class="lasttitle"><%product%> Installer</span>
            </div>

            <h2>Gossamer Threads Inc.</h2>
            <h3>End User Product License Agreement</h3>
            <h4>IMPORTANT: READ THIS LICENSE BEFORE INSTALLING THE SOFTWARE</h4>

            <p>
              This software product (the "Software") and the accompanying
              documentation (the "Documentation") (together, the "Product") are
              provided only under license from Gossamer Threads Inc. ("GTI") to
              its customers for their use only as set forth in this Agreement.
              You should carefully read the following terms and conditions
              before downloading, installing and using the Software or using
              the Documentation. Installing or otherwise using any part of the
              Software indicates that you accept these terms and conditions. If
              you do not agree with the terms and conditions of this Agreement,
              do not download, install or otherwise use the Software and do not
              click on the "I agree" or similar button. If you have received
              the Product on physical media, return the entire product with the
              software and documentation unused to the supplier where you
              obtained it.
            </p>

            <ol>
              <li>
                License:  GTI grants you a personal, non exclusive, license to
                use the Software in executable form and the Documentation,
                subject to the terms and restrictions set forth in this
                Agreement. You are not permitted to lease, rent, distribute,
                publish or sub license the Software or the Documentation or to
                use any part of the Product in a time sharing arrangement or
                in any other unauthorized manner provided that you may
                transfer all your rights in the Product to another person as
                long as you remove all copies from your computers and cease
                all use of it.  GTI does not grant you any license in the
                source code of the Software. This Agreement does not grant you
                any rights to patents, copyrights, trade secrets, trademarks
                or any other rights with respect to the Product.
              </li>
              <li>
<%~if product eq 'Gossamer Mail' and not version ends '-Single'%>
                Permitted Use.  You are authorized to have, at any time while
                this license is valid, one installation of the Software only,
                and you may remove the Software from one server and install it
                on another as long as it is running on only one server
    <%~if version ends '-Enterprise' or version ends '-Upgrade'%>
                at any time.
    <%~elsif version ends '-Professional'%>
                for a maximum of one hundred domains only at any time.
    <%~else%>
                for a maximum of five domains only at any time.
    <%~endif%>
<%~else%>
                Permitted Use.  You are authorized to have, at any time while
                this license is valid, one installation of the Software for one
                domain only, and you may remove the Software from one server
                and install it on another as long as it is running on only one
                server for one domain only at any time.
<%~endif%>
              </li>

              <li>
                Backup and Copyright Notices.  You may reproduce one copy of
                the Software and the Documentation for backup or archive
                purposes.  Any such copies must contain GTI's and its
                licensors' proprietary rights and copyright notices in the same
                form as on the original. You agree not to remove or deface any
                portion of any legend provided on any licensed program or
                documentation delivered to you under this Agreement.
              </li>

              <li>
                Modification.  You may make unlimited modifications to the
                Software for your own internal use only, but any support
                obligations of GTI with respect to the Software will be
                terminated if you do so.
              </li>

              <li>
                LIABILITY LIMITATIONS.  IN NO EVENT WILL GTI BE LIABLE TO YOU
                OR TO ANY OTHER PARTY FOR DIRECT, INDIRECT, SPECIAL, INCIDENTAL
                OR CONSEQUENTIAL DAMAGES ARISING OUT OF THE USE OF THE
                SOFTWARE, THE DOCUMENTATION OR ANY DERIVATIVES THEREOF, EVEN IF
                GTI HAS BEEN ADVISED OF THE POSSIBILITY OF SUCH DAMAGE. GTI
                SPECIFICALLY DISCLAIMS ANY WARRANTIES, INCLUDING BUT NOT
                LIMITED TO ANY IMPLIED WARRANTIES OF MERCHANTABILITY, FITNESS
                FOR A PARTICULAR PURPOSE AND NON-INFRINGEMENT. THE SOFTWARE AND
                THE DOCUMENTATION ARE PROVIDED ON AN "AS IS" BASIS, AND GTI HAS
                NO OBLIGATION TO PROVIDE MAINTENANCE, SUPPORT, UPDATES,
                ENHANCEMENTS OR MODIFICATIONS EXCEPT AS SPECIFICALLY AGREED
                UPON.
              </li>

              <li>
                Ownership.  You acknowledge and agree that the structure,
                sequence and organization of the Software are valuable trade
                secrets of GTI and that you will hold such trade secrets in
                confidence. You further acknowledge and agree that ownership of
                and title to the Product and all subsequent copies thereof,
                regardless of the form or media, are held by GTI.
              </li>

              <li>
                Indemnity.  You will indemnify and save GTI harmless from any
                and all actions, damages, liabilities, charges, claims and
                associated expenses ("Claims") against or incurred by GTI in
                any way connected with your use of the Product, whether such
                Claims are by you or by any third parties as a result of or
                related to your use of the Software.
              </li>

              <li>
                Termination.  The licenses granted hereunder are perpetual
                unless terminated earlier as specified below. You may terminate
                the licenses and this Agreement at any time by destroying the
                Software and the Documentation together with all copies and
                merged portions in any form. The licenses and this Agreement
                will also terminate immediately and automatically without
                notice if you fail to comply with any term or condition of this
                Agreement. Upon such termination you agree to destroy the
                Software and the Documentation, together with all copies and
                merged portions in any form. GTI will not be liable for any
                refund to you on termination of this Agreement for any reason.
              </li>

              <li>
                Government Use.  If you are acquiring the Software on behalf of
                the U.S. government, the Government shall have only "Restricted
                Rights" in the Software and the Documentation as defined in
                clause 52.227 19(c)(2) of the U.S. Federal Acquisition
                Regulations.
              </li>

              <li>
                Severability.  If any provision of this Agreement is found to
                be invalid, illegal or unenforceable, the validity, legality
                and enforceability of any of the remaining provisions shall not
                in any way be affected or impaired and a valid, legal and
                enforceable provision of similar intent and economic impact
                shall be substituted therefor.
              </li>

              <li>
                Entire Agreement: This Agreement sets forth the entire
                understanding and agreement between you and GTI and supersedes
                all prior agreements with respect to the Product. GTI may
                change the terms of this Agreement by electronic notice to you.
              </li>

              <li>
                Governing Law.  This Agreement shall be governed by the laws of
                the Province of British Columbia and the federal laws of Canada
                applicable therein excluding its conflicts of laws principles
                and excluding the United Nations Convention on Contracts for
                the International Sale of Goods.
              </li>
            </ol>

            <form action="install.cgi" method="post">
              <input type="hidden" name="lite" value="<%if lite%><%lite%><%endif%>" />
              <input type="hidden" name="eula_displayed" value="1" />
              <%if in.upgrade_choice and in.install_dir%>
              <input type="hidden" name="upgrade_choice" value="<%in.upgrade_choice%>" />
              <input type="hidden" name="install_dir" value="<%in.install_dir%>" />
              <%endif%>
              <div class="row required clear">
                <label for="accept_eula" class="name" style="width: auto">I have read and accept the terms of the agreement: <span>*</span></label>
                <div class="value">
                  <input type="checkbox" id="accept_eula" name="accept_eula" class="checkbox" />
                </div>
              </div>

              <div class="formsubmit">
                <input type="submit" value="Next &gt;&gt;" class="submit" />
              </div>
            </form>

          </div>
        </div>
        <div class="shadowbottom"><div class="shadowbottomleft"></div><div class="shadowbottomright"></div></div>
      </div>
      <hr class="hide" />
      <div id="footer" class="clear">
        <a href="http://www.gossamer-threads.com"><em id="powered_by" title="Powered by <%product%>"></em></a>
        <p>&copy; 2008 Gossamer Threads Inc.</p>
      </div>
    </div>
  </body>
</html>
EULA
    FIRST_SCREEN => <<'FIRST_SCREEN',
<!DOCTYPE html PUBLIC "-//W3C//DTD XHTML 1.0 Transitional//EN" "http://www.w3.org/TR/xhtml1/DTD/xhtml1-transitional.dtd">
<html>
  <head>
    <title><%product%> <%version%> - Installation</title>
    <%css%>
    <script type="text/javascript">
      function updatePathBox() {
        document.getElementById('existing_admin_path').style.display = document.getElementById('upgrade_install').checked ? 'block' : 'none';
      }
      window.onload = function () {
        updatePathBox();
        document.getElementById('new_install').onclick = updatePathBox;
        document.getElementById('upgrade_install').onclick = updatePathBox;
      }
    </script>
  </head>
  <body>
    <div class="wrapper">
      <div id="header">
        <div id="logo"><h1><a href="http://www.gossamer-threads.com"><%product%></a></h1></div>
      </div>

      <hr class="hide" />
      <div class="bar1"></div>
      <div class="bar2"></div>

      <%if error or message~%>
      <div id="contentheader">
        <%if error%><div class="error"><%error%></div><%endif%>
        <%if message%><div class="message"><%message%></div><%endif%>
      </div>
      <%~endif%>

      <div class="shadowleft">
        <div class="shadowtop"><div class="shadowtopleft"></div><div class="shadowtopright"></div></div>
        <div class="shadowright">
          <div class="content">

            <div class="crumb">
              <span class="lasttitle"><%product%> Installer</span>
            </div>

            <h2><%product%> Installer</h2>

            <p>
              Welcome to <%product%>.  This program will unarchive and install <%product%>.
            </p>

            <form action="install.cgi" method="post">
              <input type="hidden" name="lite" value="<%if lite%><%lite%><%endif%>" />
              <input type="hidden" name="accept_eula" value="<%if in.accept_eula%><%in.accept_eula%><%endif%>" />

              <div class="row clear">
                <label for="new_install" class="name">New installation:</label>
                <div class="value">
                  <input type="radio" id="new_install" name="upgrade_choice" value="No"<%unless in.upgrade_choice and in.upgrade_choice eq 'Yes'%> checked="checked"<%endunless%> />
                </div>
              </div>
              <div class="row clear">
                <label for="upgrade_install" class="name">Upgrade existing installation:</label>

                <div class="value">
                  <input type="radio" id="upgrade_install" name="upgrade_choice" value="Yes"<%if in.upgrade_choice and in.upgrade_choice eq 'Yes'%> checked="checked"<%endif%> />
                </div>
              </div>
              <div id="existing_admin_path" class="row required clear">
                <label for="install_dir" class="name">Path to existing installation admin: <span>*</span></label>
                <div class="value">
                  <input type="text" id="install_dir" name="install_dir" class="text"<%if in.install_dir%> value="<%in.install_dir%>"<%endif%> />
                </div>
              </div>

              <div class="formsubmit">
                <input type="submit" value="Next &gt;&gt;" class="submit" />
              </div>
            </form>

          </div>
        </div>
        <div class="shadowbottom"><div class="shadowbottomleft"></div><div class="shadowbottomright"></div></div>
      </div>
      <hr class="hide" />
      <div id="footer" class="clear">
        <a href="http://www.gossamer-threads.com"><em id="powered_by" title="Powered by <%product%>"></em></a>
        <p>&copy; 2008 Gossamer Threads Inc.</p>
      </div>
    </div>
  </body>
</html>
FIRST_SCREEN
    UPGRADE_FIRST_SCREEN => <<'UPGRADE_FIRST_SCREEN',
<!DOCTYPE html PUBLIC "-//W3C//DTD XHTML 1.0 Transitional//EN" "http://www.w3.org/TR/xhtml1/DTD/xhtml1-transitional.dtd">
<html>
  <head>
    <title><%product%> <%version%> - Installation - Upgrade existing installation</title>
    <%css%>
  </head>
  <body>
    <div class="wrapper">
      <div id="header">
        <div id="logo"><h1><a href="http://www.gossamer-threads.com"><%product%></a></h1></div>
      </div>

      <hr class="hide" />
      <div class="bar1"></div>
      <div class="bar2"></div>

      <%if error or message~%>
      <div id="contentheader">
        <%if error%><div class="error"><%error%></div><%endif%>
        <%if message%><div class="message"><%message%></div><%endif%>
      </div>
      <%~endif%>

      <div class="shadowleft">
        <div class="shadowtop"><div class="shadowtopleft"></div><div class="shadowtopright"></div></div>
        <div class="shadowright">
          <div class="content">

            <div class="crumb">
              <a href="install.cgi?upgrade_choice=Yes;install_dir=<%escape_url unescape_html in.install_dir%>;cgi_first_screen=1"><%product%> Installer</a> &gt;
              <span class="lasttitle">Upgrade existing installation</span>
            </div>

            <h2>Upgrade existing installation</h2>

            <p>
              Welcome to <%product%>.  This program will unarchive and install
              your <%product%> upgrade.  The installation located at the path
              you provided, '<%in.install_dir%>', will be updated to
              <%product%> <%version%>.
            </p>

            <form action="install.cgi" method="post">
              <input type="hidden" name="lite" value="<%if lite%><%lite%><%endif%>" />
              <input type="hidden" name="accept_eula" value="<%if in.accept_eula%><%in.accept_eula%><%endif%>" />
              <input type="hidden" name="upgrade_second" value="1" />
              <input type="hidden" name="install_dir" value="<%GT_ADMIN_PATH%>" />

<%loop rows%>
<%if skip%><%nextloop%><%endif%>
<%if type and type eq 'message'%>
              <div class="row clear"><%message%></div>
<%else%>
              <div class="row clear">
                <label for="option<%row_num - 1%>" class="name"><%message%></label>
                <div class="value">
                  <select name="<%row_num - 1%>">
                    <option value="o"><%GT::Installer::tpllang('overwrite')%></option>
                    <option value="b"><%GT::Installer::tpllang('backup')%></option>
                    <option value="s"><%GT::Installer::tpllang('skip')%></opion>
                  </select>
                </div>
              </div>
<%endif%>
<%endloop%>

<%if fields.length%>
              <div class="row clear">The following fields are new <%product%> fields and need to be set before continuing.  Appropriate defaults have been chosen where possible.</div>
<%~loop fields%>
              <%~if type eq 'message'%>
              <div class="row clear"><%message%></div>
              <%~else%>
              <div class="row<%if required%> required<%endif%> clear">
                <label for="<%key%>" class="name"><%message%><%if required%> <span>*</span><%endif%></label>
                <div class="value">
                  <%~if type eq 'create_dirs%>
                  <input type="checkbox" id="<%key%>" name="<%key%>" value="1" class="checkbox"<%if value%> checked="checked"<%endif%> />
                  <%~else%>
                  <input type="<%if type eq 'password'%>password<%else%>text<%endif%>" id="<%key%>" name="<%key%>" value="<%if value%><%value%><%endif%>" class="<%if type eq 'password'%>password<%else%>text<%endif%>" />
                  <%~endif%>
                </div>
              </div>
              <%~endif%>
<%~endloop%>
<%endif%>

              <div class="formsubmit">
                <input type="submit" value="Next &gt;&gt;" class="submit" />
              </div>
            </form>

          </div>
        </div>
        <div class="shadowbottom"><div class="shadowbottomleft"></div><div class="shadowbottomright"></div></div>
      </div>
      <hr class="hide" />
      <div id="footer" class="clear">
        <a href="http://www.gossamer-threads.com"><em id="powered_by" title="Powered by <%product%>"></em></a>
        <p>&copy; 2008 Gossamer Threads Inc.</p>
      </div>
    </div>
  </body>
</html>
UPGRADE_FIRST_SCREEN
  UPGRADE_SECOND_SCREEN_FIRST => <<'UPGRADE_SECOND_SCREEN_FIRST',
<!DOCTYPE html PUBLIC "-//W3C//DTD XHTML 1.0 Transitional//EN" "http://www.w3.org/TR/xhtml1/DTD/xhtml1-transitional.dtd">
<html>
  <head>
    <title><%product%> <%version%> - Installation - Upgrade existing installation - Upgrading</title>
    <%css%>
  </head>
  <body>
    <div class="wrapper">
      <div id="header">
        <div id="logo"><h1><a href="http://www.gossamer-threads.com"><%product%></a></h1></div>
      </div>

      <hr class="hide" />
      <div class="bar1"></div>
      <div class="bar2"></div>

      <div class="shadowleft">
        <div class="shadowtop"><div class="shadowtopleft"></div><div class="shadowtopright"></div></div>
        <div class="shadowright">
          <div class="content">

            <div class="crumb">
              <%product%> Installer &gt; Upgrade existing installation &gt; <span class="lasttitle">Upgrading...</span>
            </div>

            <h2>Upgrading...</h2>

            <p class="no-bmargin">
              <%product%> is now being upgraded to <%version%>.  Please be
              patient, and do not hit stop.
            </p>

          </div>
        </div>
        <div class="shadowbottom"><div class="shadowbottomleft"></div><div class="shadowbottomright"></div></div>
      </div>
      <hr class="hide" />
    </div>

<blockquote>
<pre>
UPGRADE_SECOND_SCREEN_FIRST
    UPGRADE_SECOND_SCREEN_SECOND => <<'UPGRADE_SECOND_SCREEN_SECOND',
</pre>
</blockquote>

    <div class="wrapper">

      <hr class="hide" />

      <div class="shadowleft">
        <div class="shadowtop"><div class="shadowtopleft"></div><div class="shadowtopright"></div></div>
        <div class="shadowright">
          <div class="content">
            <h2 style="margin-top: 0px">Upgrade complete</h2>

            <p>
              <%product%> has been successfully upgraded to <%version%>.
            </p>

            <p>
              Please do not leave your original .tar.gz file in your web directory!
            </p>

            <p class="no-bmargin">
              If you have any problems, please visit our <a href="http://www.gossamer-threads.com/perl/forum/">support forum</a>.
            </p>
          </div>
        </div>
        <div class="shadowbottom"><div class="shadowbottomleft"></div><div class="shadowbottomright"></div></div>
      </div>
      <hr class="hide" />
      <div id="footer" class="clear">
        <a href="http://www.gossamer-threads.com"><em id="powered_by" title="Powered by <%product%>"></em></a>
        <p>&copy; 2008 Gossamer Threads Inc.</p>
      </div>
    </div>
  </body>
</html>
UPGRADE_SECOND_SCREEN_SECOND
    INSTALL_WARNING => '<p><b>WARNING:</b> Please remove the install.cgi and install.dat file from this directory. It is a security risk to leave those files here.</p>',
    INSTALL_REMOVED => '<p>The install files have been removed. If you need to re-run the install, please unarchive the original file again.</p>',
    OVERWRITE => 'Overwrite',
    BACKUP => 'Backup',
    SKIP => 'Skip',
    INSTALL_FIRST_SCREEN => <<'INSTALL_FIRST_SCREEN',
<!DOCTYPE html PUBLIC "-//W3C//DTD XHTML 1.0 Transitional//EN" "http://www.w3.org/TR/xhtml1/DTD/xhtml1-transitional.dtd">
<html>
  <head>
    <title><%product%> <%version%> - Installation - New installation</title>
    <%css%>
  </head>
  <body>
    <div class="wrapper">
      <div id="header">
        <div id="logo"><h1><a href="http://www.gossamer-threads.com"><%product%></a></h1></div>
      </div>

      <hr class="hide" />
      <div class="bar1"></div>
      <div class="bar2"></div>

      <%if error or message~%>
      <div id="contentheader">
        <%if error%><div class="error"><%error%></div><%endif%>
        <%if message%><div class="message"><%message%></div><%endif%>
      </div>
      <%~endif%>

      <div class="shadowleft">
        <div class="shadowtop"><div class="shadowtopleft"></div><div class="shadowtopright"></div></div>
        <div class="shadowright">
          <div class="content">

            <div class="crumb">
              <a href="install.cgi?upgrade_choice=No;cgi_first_screen=1"><%product%> Installer</a> &gt;
              <span class="lasttitle">New installation</span>
            </div>

            <h2>New installation</h2>

            <p>
              Welcome to <%product%>.  This program will unarchive and install
              <%product%> <%version%>.  In order to proceed, you need to
              provide the following information.  Sensible defaults have been
              chosen where possible, but please double-check that they are
              correct.
            </p>

            <form action="install.cgi" method="post">
              <input type="hidden" name="lite" value="<%if lite%><%lite%><%endif%>" />
              <input type="hidden" name="accept_eula" value="<%if in.accept_eula%><%in.accept_eula%><%endif%>" />
              <input type="hidden" name="install" value="1" />

              <%~loop fields%>
              <%~if type eq 'message'%>
              <div class="row clear"><%message%></div>
              <%~else%>
              <div class="row<%if required%> required<%endif%> clear">
                <label for="<%key%>" class="name"><%message%><%if required%> <span>*</span><%endif%></label>
                <div class="value">
                  <%~if type eq 'create_dirs%>
                  <input type="checkbox" id="<%key%>" name="<%key%>" value="1" class="checkbox"<%if value%> checked="checked"<%endif%> />
                  <%~else%>
                  <input type="<%if type eq 'password'%>password<%else%>text<%endif%>" id="<%key%>" name="<%key%>" value="<%if value%><%value%><%endif%>" class="<%if type eq 'password'%>password<%else%>text<%endif%>" />
                  <%~endif%>
                </div>
              </div>
              <%~endif%>
              <%~endloop%>

              <div class="formsubmit">
                <input type="submit" value="Next &gt;&gt;" class="submit" />
              </div>
            </form>

          </div>
        </div>
        <div class="shadowbottom"><div class="shadowbottomleft"></div><div class="shadowbottomright"></div></div>
      </div>
      <hr class="hide" />
      <div id="footer" class="clear">
        <a href="http://www.gossamer-threads.com"><em id="powered_by" title="Powered by <%product%>"></em></a>
        <p>&copy; 2008 Gossamer Threads Inc.</p>
      </div>
    </div>
  </body>
</html>
INSTALL_FIRST_SCREEN
    INSTALL_SECOND_SCREEN_FIRST => <<'INSTALL_SECOND_SCREEN_FIRST',
<!DOCTYPE html PUBLIC "-//W3C//DTD XHTML 1.0 Transitional//EN" "http://www.w3.org/TR/xhtml1/DTD/xhtml1-transitional.dtd">
<html>
  <head>
    <title><%product%> <%version%> - Installation - New installation - Installing</title>
    <%css%>
  </head>
  <body>
    <div class="wrapper">
      <div id="header">
        <div id="logo"><h1><a href="http://www.gossamer-threads.com"><%product%></a></h1></div>
      </div>

      <hr class="hide" />
      <div class="bar1"></div>
      <div class="bar2"></div>

      <div class="shadowleft">
        <div class="shadowtop"><div class="shadowtopleft"></div><div class="shadowtopright"></div></div>
        <div class="shadowright">
          <div class="content">

            <div class="crumb">
              <%product%> Installer &gt; New installation &gt; <span class="lasttitle">Installing...</span>
            </div>

            <h2>Installing...</h2>

            <p class="no-bmargin">
              <%product%> <%version%> is now being installed.  Please be
              patient, and do not hit stop.
            </p>

          </div>
        </div>
        <div class="shadowbottom"><div class="shadowbottomleft"></div><div class="shadowbottomright"></div></div>
      </div>
      <hr class="hide" />
    </div>

<blockquote>
<pre>
INSTALL_SECOND_SCREEN_FIRST
    INSTALL_SECOND_SCREEN_SECOND => <<'INSTALL_SECOND_SCREEN_SECOND',
</pre>
</blockquote>

    <div class="wrapper">

      <hr class="hide" />

      <div class="shadowleft">
        <div class="shadowtop"><div class="shadowtopleft"></div><div class="shadowtopright"></div></div>
        <div class="shadowright">
          <div class="content">
            <h2 style="margin-top: 0px">Installation complete</h2>

            <p>
              <%product%> <%version%> has been successfully installed.
            </p>

            <%if install_message%><p><%install_message%></p><%endif%>

            <p>
              Please do not leave your original .tar.gz file in your web directory!
            </p>

            <p<%unless message%> class="no-bmargin"<%endif%>>
              If you have any problems, please visit our <a href="http://www.gossamer-threads.com/perl/forum/">support forum</a>.
            </p>

            <%if message%><p class="no-bmargin complete-installation"><%message%></p><%endif%>

          </div>
        </div>
        <div class="shadowbottom"><div class="shadowbottomleft"></div><div class="shadowbottomright"></div></div>
      </div>
      <hr class="hide" />
      <div id="footer" class="clear">
        <a href="http://www.gossamer-threads.com"><em id="powered_by" title="Powered by <%product%>"></em></a>
        <p>&copy; 2008 Gossamer Threads Inc.</p>
      </div>
    </div>
  </body>
</html>
INSTALL_SECOND_SCREEN_SECOND
    CGI_ERROR_SCREEN => <<'CGI_ERROR_SCREEN',
<%if error_breakout%><%error_breakout%><%endif%>
    <div class="wrapper">

      <hr class="hide" />

      <div class="shadowleft">
        <div class="shadowtop"><div class="shadowtopleft"></div><div class="shadowtopright"></div></div>
        <div class="shadowright">
          <div class="content">
            <h2 class="error">Error</h2>

            <p>
              An error occurred:
            </p>

            <p>
              <%error%>
            </p>
          </div>
        </div>
        <div class="shadowbottom"><div class="shadowbottomleft"></div><div class="shadowbottomright"></div></div>
      </div>
      <hr class="hide" />
      <div id="footer" class="clear">
        <a href="http://www.gossamer-threads.com"><em id="powered_by" title="Powered by <%product%>"></em></a>
        <p>&copy; 2008 Gossamer Threads Inc.</p>
      </div>
    </div>
  </body>
</html>
CGI_ERROR_SCREEN
    INVALID_RESPONCE => "\nInvalid Response (%s)\n",
);

# vim:ft=perl


%INST::LANG = (
    prompt_warning => q|<b style="color: red">WARNING</b>: This installer will not upgrade from FileMan 1.x!|,
    prompt_install => q|You have selected to perform a fresh install. Please answer the following questions, you can hit enter to accept the default value shown in brackets.|,
    prompt_cgipath => q|Please enter the system path (directory on your server) where FileMan should store its cgi files. No trailing slash please.|,
    prompt_cgiurl => q|Please enter the URL to the directory you just entered. No trailing slash please.|,
    prompt_privatepath => q|Please enter the private path (directory on your server) where FileMan should store its data, library and config files. No trailing slash please.|,
    prompt_staticpath => q|Please enter the system path (directory on your server) where FileMan should store its css, js and images files. This should NOT be inside the cgi-bin directory. No trailing slash please.|,
    prompt_staticurl => q|Please enter the URL to the static directory you just entered.  No trailing slash please.  This field may contain a relative URL.|,
    prompt_rootpath => q|Please enter the path to the root directory where FileMan will be restricted to. This means FileMan will display all files within this folder. No trailing slash please.|,
    prompt_perlpath => q|Please enter the path to perl. The default should be ok if you are not sure.|,
    prompt_security => q|The install script will setup a username/password to protect your FileMan installation. Please choose a secure username and password to ensure your FileMan installation isn't easily compromised.|,
    prompt_username => q|Please enter your FileMan admin username.|,
    prompt_password => q|Please enter your FileMan password.|,
    prompt_email => q|Please enter the admin e-mail address. This address will be used in all e-mail correspondence.|,
    prompt_mailserver => q|Please enter either the path to sendmail, or the SMTP server to use when sending e-mail.|,
    prompt_regnumber => q|Please enter your FileMan registration number.|,
    prompt_success => q|FileMan has been successfully installed. To access the program, please visit:<br />
<br />
<a href="<%CGI URL%>/fileman.cgi"><%CGI URL%>/fileman.cgi</a><br />
Username: <%Username%><br />
Password: <%Password%><br />
|,
    prompt_upgrade => q|Do you want the installer to update your program files? It will overwrite all existing files and templates.|,
    error_checksum => q|\nWarning: You do not have a valid checksum file; backups will not be made.\n|,
    error_configfile => q|Could not load your config file:|,

    TELNET_EULA => <<'EULA',
GOSSAMER THREADS INC.
END USER PRODUCT LICENSE AGREEMENT

IMPORTANT:	READ THIS LICENSE BEFORE INSTALLING THE SOFTWARE

This program is being distributed as shareware.  It may be used and
modified free of charge for personal, academic or non-profit
use, so long as all copyright notices and links to gossamer-threads.com
in the template and code are not altered.

Commercial users may use this program under the same terms as long as it
used solely for internal use only (i.e. maintaing your own website). If you
wish to use this in a control panel for an ISP, or as a file manager for
free web space, or similiar activites, please contact Gossamer Threads
for permission.

Selling the code for this program without prior written consent is
expressly forbidden.  Written permission must be obtained
before redistributing this program over the Internet or in any
other medium.  In all cases copyright and header must remain intact.

By using this program, you agree to indemnify Gossamer Threads Inc.
from any liability.

For any questions on the licensing agreement, please contact
Gossamer Threads Inc at:

http://www.gossamer-threads.com/contact/

EULA
    HTML_EULA => <<'EULA',
<!DOCTYPE html PUBLIC "-//W3C//DTD XHTML 1.0 Transitional//EN" "http://www.w3.org/TR/xhtml1/DTD/xhtml1-transitional.dtd">
<html>
  <head>
    <title><%product%> <%version%> - Installation - End User Product License Agreement</title>
    <%css%>
    <style type="text/css">
/*<![CDATA[*/
      .eula ol {
        list-style-type: decimal;
      }
/*]]>*/
    </style>
  </head>
  <body>
    <div class="wrapper">
      <div id="header">
        <div id="logo"><h1><a href="http://www.gossamer-threads.com"><%product%></a></h1></div>
      </div>

      <hr class="hide" />
      <div class="bar1"></div>
      <div class="bar2"></div>

      <%if in.eula_displayed~%>
      <div id="contentheader">
        <div class="error"><%GT::Installer::tpllang('eula_required')%></div>
      </div>
      <%~endif%>

      <div class="shadowleft">
        <div class="shadowtop"><div class="shadowtopleft"></div><div class="shadowtopright"></div></div>
        <div class="shadowright">
          <div class="content eula">

            <div class="crumb">
              <span class="lasttitle"><%product%> Installer</span>
            </div>

            <h2>Gossamer Threads Inc.</h2>
            <h3>End User Product License Agreement</h3>
            <h4>IMPORTANT: READ THIS LICENSE BEFORE INSTALLING THE SOFTWARE</h4>

            <p>
              This program is being distributed as shareware.  It may be used and
              modified free of charge for personal, academic or non-profit
              use, so long as all copyright notices and links to gossamer-threads.com
              in the template and code are not altered.
            </p>

            <p>
              Commercial users may use this program under the same terms as long as it
              used solely for internal use only (i.e. maintaing your own website). If you
              wish to use this in a control panel for an ISP, or as a file manager for
              free web space, or similiar activites, please contact Gossamer Threads
              for permission.
            </p>

            <p>
              Selling the code for this program without prior written consent is
              expressly forbidden.  Written permission must be obtained
              before redistributing this program over the Internet or in any
              other medium.  In all cases copyright and header must remain intact.
            </p>

            <p>
              By using this program, you agree to indemnify Gossamer Threads Inc.
              from any liability.
            </p>

            <p>
              For any questions on the licensing agreement, please contact
              Gossamer Threads Inc at:
            </p>

            <p>
              http://www.gossamer-threads.com/contact/
            </p>

            <form action="install.cgi" method="post">
              <input type="hidden" name="lite" value="<%if lite%><%lite%><%endif%>" />
              <input type="hidden" name="eula_displayed" value="1" />
              <%if in.upgrade_choice and in.install_dir%>
              <input type="hidden" name="upgrade_choice" value="<%in.upgrade_choice%>" />
              <input type="hidden" name="install_dir" value="<%in.install_dir%>" />
              <%endif%>
              <div class="row required clear">
                <label for="accept_eula" class="name" style="width: auto">I have read and accept the terms of the agreement: <span>*</span></label>
                <div class="value">
                  <input type="checkbox" id="accept_eula" name="accept_eula" class="checkbox" />
                </div>
              </div>

              <div class="formsubmit">
                <input type="submit" value="Next &gt;&gt;" class="submit" />
              </div>
            </form>

          </div>
        </div>
        <div class="shadowbottom"><div class="shadowbottomleft"></div><div class="shadowbottomright"></div></div>
      </div>
      <hr class="hide" />
      <div id="footer" class="clear">
        <a href="http://www.gossamer-threads.com"><em id="powered_by" title="Powered by <%product%>"></em></a>
        <p>&copy; 2008 Gossamer Threads Inc.</p>
      </div>
    </div>
  </body>
</html>
EULA
);


#--END Libs
}

use strict;
use lib;
use vars qw/%VERSION_TREE %DEFAULT_PERMISSION/;

# This has to be updated every release so that an upgrade can "walk" the tree
# to find any upgrade code.  The format should be:
# '1_0_0' => '1_0_1'

%DEFAULT_PERMISSION = ( chmod => '1', command => '1', compress => '1', copy => '1', delete => '1', diff => '1', download => '1', edit => '1', makedir => '1', move => '1', newfile => '1', perl => '1', preferences => '1', protect => '1', rename => '1', replace => '1', search => '1', symlink => '1', tail => '1', upload => '1' );
%VERSION_TREE = (
    map { $_ => '3_0_0' } qw/2_0_0 2_0_1 2_0_2 2_0_3 2_0_4 2_1_0 2_1_1/
);

main();

sub main {
# ---------------------------------------------------------------
# This is the main code loop. It is ran for every request in CGI
# mode. All configuration is set up by this function.
#
    my $welcome_format = 'professional';
    my $format         = 'plain';
    my $version_type   = 'single';
    my $cfg_path       = '<%Private Path%>/fileman.conf';
    my $i = GT::Installer->new(
        product        => 'FileMan',
        version        => '3.0.0',
        load_defaults  => \&load_defaults,
        tar_checksum   => 'f71789f11e92533699934321ad0c93fa',
        load_config    => \&load_config,
        save_config    => \&save_config,
        checksums      => "<%Private Path%>/checksums.dat",
        save_cache     => 0,
        replace_path   => {
            '../fileman.conf' => $cfg_path
        }
    );

# FileMan uses a different license than our other products.
    $GT::Installer::LANG{TELNET_EULA} = $INST::LANG{TELNET_EULA};
    $GT::Installer::LANG{HTML_EULA} = $INST::LANG{HTML_EULA};

    $i->{config}->{fversion} = $version_type;

# Display warning.
    $i->initial_message($INST::LANG{prompt_warning}, $format);

# Display New Install Message
    $i->add_config_message($INST::LANG{prompt_install}, $format);

# User CGI directory.
    $i->add_config_message($INST::LANG{prompt_cgipath}, $format);
    $i->add_config(
        type            => 'path',
        key             => "CGI Path",
        message         => 'CGI Path',
    );
    $i->add_config_message($INST::LANG{prompt_cgiurl}, $format);
    $i->add_config(
        type            => 'url',
        key             => "CGI URL",
        message         => 'CGI URL',
        telnet_callback => \&telnet_callback
    );
# Private directory.
    $i->add_config_message($INST::LANG{prompt_privatepath}, $format);
    $i->add_config(
        type            => 'path',
        key             => "Private Path",
        message         => 'Private Path',
    );

# Static directory and URL.
    $i->add_config_message($INST::LANG{prompt_staticpath}, $format);
    $i->add_config(
        type            => 'path',
        key             => "Static Path",
        message         => 'Static Path',
    );
    $i->add_config_message($INST::LANG{prompt_staticurl}, $format);
    $i->add_config(
        type            => 'url',
        key             => "Static URL",
        message         => 'Static URL',
    );

# Path to root directory
    $i->add_config_message($INST::LANG{prompt_rootpath}, $format);
    $i->add_config(
        type            => 'path',
        key             => "Root Path",
        message         => 'Root Path',
    );

# Path to perl
    $i->add_config_message($INST::LANG{prompt_perlpath}, $format);
    $i->add_config(
        type            => 'perl_path',
        message         => 'Path to Perl'
    );

    $i->add_config_message($INST::LANG{prompt_security}, $format);

# Username
    $i->add_config_message($INST::LANG{prompt_username}, $format);
    $i->add_config(
        type            => 'text',
        message         => 'Username',
        key             => 'Username'
    );
# Password
    $i->add_config_message($INST::LANG{prompt_password}, $format);
    $i->add_config(
        type            => 'password',
        message         => 'Password',
        key             => "Password",
    );

# For multiple version
    if ($version_type eq 'multiple') {
    # Email Address
        $i->add_config_message($INST::LANG{prompt_email}, $format);
        $i->add_config(
            type            => 'email',
            key             => "Admin Email",
            message         => 'Admin Email',
        );

    # Email Server
        $i->add_config_message($INST::LANG{prompt_mailserver}, $format);
        $i->add_config(
            type            => 'email_support',
            message         => 'SMTP/Sendmail',
            key             => 'SMTP/Sendmail'
        );

    # Registration Number.
        $i->add_config_message($INST::LANG{prompt_regnumber}, $format);
        $i->add_config(
            type            => 'reg_number',
            message         => 'Registration Number'
        );

    }

# The exit message upon a successfull install.
    $i->install_exit_message($INST::LANG{prompt_success}, $format);

# Regex to determine what gets put where.
    $i->install_to(
        '^cgi/(.*)$'       => 'CGI Path',
        '^static/(.*)$'    => 'Static Path',
        '^(lib/.*)$'       => 'Private Path',
        '^(sessions/.*)$'  => 'Private Path',
        '^(templates/.*)$' => 'Private Path',
        '^(tmp/.*)$'       => 'Private Path',
        '^(fileman.*)$'    => 'Private Path',
    );

# Get to specify what the use lib line should point to
    $i->use_lib('<%Private Path%>'.'/lib');
    $i->add_upgrade_message($INST::LANG{prompt_upgrade}, $format);

    if ($version_type eq 'multiple') {
        $i->add_upgrade(skip => ['fileman.conf', '\.htaccess', '\.htpasswd', 'fileman.log', 'fileman.dat']);
    }
    else {
        $i->add_upgrade(skip => ['fileman.conf', '\.htaccess', '\.htpasswd']);
    }
    $i->perform;
}

sub _check_checksum {
# ---------------------------------------------------------------
    my $i = shift;
    my $checksum = $i->{config}->{'Private Path'} . '/checksums.dat';
    if (! -e $checksum) {
        if ($i->{upgrade}->[3]->{answer} =~ /^b/i) {
            $i->print($INST::LANG{error_checksum});
        }
    }
    return 1;
}

sub load_defaults {
# ---------------------------------------------------------------
    my ($i) = @_;
    if ($i->{is_cgi}) {
        my $url = $i->{in}->url( absolute => 1, query_string => 0 );
        my $index = rindex($url, '/');
        my $u = substr($url, 0, $index);
        $i->{defaults}->{"CGI URL"}         ||= $u;
        if ($u !~ /cgi/i) {
            $i->{defaults}->{"Static URL"}  ||= "$u/static";
        }
    }
    my $path = GT::Installer->find_cgi;
    $i->{defaults}->{"CGI Path"}            ||= $path;
    if ($path !~ /cgi/i) {
        $i->{defaults}->{"Static Path"}     ||= "$path/static";
    }
    $i->{defaults}->{"Private Path"}        ||= "$path/private";
    $i->{defaults}->{"Root Path"}           ||= $path;
    $i->{defaults}->{"SMTP/Sendmail"}       ||= GT::Installer->find_sendmail();
    $i->{defaults}->{"Path to Perl"}        ||= GT::Installer->find_perl();
    return 1;
}

sub load_config {
# ---------------------------------------------------------------
    my ($i) = @_;

    my $gt_path = $i->{config}->{GT_ADMIN_PATH};
    my $path    = "$gt_path/private/fileman.conf";
    unless (-e $path) {
        if ($i->{config}{version} =~ /^3/) {
            $path = "$gt_path/private/fileman.conf";
        }
        else {
            $path = $i->{config}->{fversion} eq 'multiple' ? "$gt_path/private/lib/FileMan/ConfigData.pm" : "$gt_path/private/ConfigData.pm";
        }
    }
    -e $path or return $i->error($INST::LANG{error_configfile}.$path, 'WARN');

    local($@, $!);
    my $cfg = do $path;
    if ($@ || $!) {
        return $i->error($INST::LANG{error_configfile} . $@ || $! , 'WARN');
    }
    $i->{config}->{"CGI Path"}            ||= $cfg->{root_path}    || $cfg->{cgi_path};
    $i->{config}->{"CGI URL"}             ||= $cfg->{root_url}     || $cfg->{cgi_url};
    $i->{config}->{"Static Path"}         ||= $cfg->{static_path}  || "$cfg->{image_path}/static";
    $i->{config}->{"Static URL"}          ||= $cfg->{static_url};
    $i->{config}->{"Private Path"}        ||= $cfg->{priv_path}    || $cfg->{private_path};
    $i->{config}->{"Path to Perl"}        ||= $cfg->{path_to_perl};

# For multiple version
    if ($i->{config}->{fversion} eq 'multiple') {
        $i->{config}->{"SMTP/Sendmail"}         = $cfg->{mail_path} || $cfg->{smtp_server};
        $i->{config}->{"Registration Number"} ||= $cfg->{reg_number};
    }
# For single version
    elsif ($i->{config}->{fversion} eq 'single') {
        $i->{config}->{login} ||= { username => $cfg->{username} || $cfg->{login}->{username}, password => $cfg->{password} || $cfg->{login}->{password} };
    }
    return 1;
}

sub save_config {
# ---------------------------------------------------------------
    my $i = shift;
    my $path = $i->{config}->{'Private Path'} . '/fileman.conf';
    if ($i->{installing}) { # Do a fresh install
        my %cfg;
        $cfg{version}            = $i->{version};
        $cfg{path_to_perl}       = $i->{config}->{'Path to Perl'};
        $cfg{cgi_path}           = $i->{config}->{'CGI Path'};
        $cfg{cgi_url}            = $i->{config}->{'CGI URL'} . '/fileman.cgi';
        $cfg{static_path}        = $i->{config}->{'Static Path'};
        $cfg{static_url}         = $i->{config}->{"Static URL"};
        $cfg{private_path}       = $i->{config}->{'Private Path'};
        $cfg{tmp_path}           = "$i->{config}->{'Private Path'}/tmp";
        $cfg{root_path}          = $i->{config}->{'Root Path'};
        $cfg{fversion}           = $i->{config}->{fversion};
        $cfg{filename_check}     = $^O =~ /mswin/i ? 0 : 1;
        $cfg{debug}              = '0';
        $cfg{template}           = 'luna';
        $cfg{command_timeout}    = 20;

# Date formats and default values
        $cfg{date}    = { display => '%dd%-%mmm%-%yy% %hh%:%MM%:%ss%', input => '%yyyy%-%mm%-%dd%' };
        $cfg{default} = { upload_mode => '644' };
        $cfg{session} = { cookie => 'session_fileman', expiry => '3' };

        if ($cfg{fversion} eq 'multiple') {
            $cfg{reg_number}     = $i->{config}->{'Registration Number'};
            $cfg{htpasswd_level} = '0';
            $cfg{history_exp}    = '7';
            $cfg{default}        = { permission => \%DEFAULT_PERMISSION, allowed_space => '5000000', upload_mode => '644' };
            $cfg{email}          = {
                admin        => $i->{config}->{'Admin Email'},
                mail_path    => ($i->{config}->{email_support} eq 'sendmail') ? $i->{config}->{'SMTP/Sendmail'} : '',
                smtp_server  => ($i->{config}->{email_support} eq 'sendmail') ? '' : $i->{config}->{'SMTP/Sendmail'}
            };

            my $fh = \do{ local *FH; *FH };
            my $admin_line = join('|', ('1', $i->{config}->{'Username'}, encrypt($i->{config}->{'Password'}), $i->{config}->{'Username'},$i->{config}->{'Admin Email'}));
            open $fh, ">$i->{config}->{'Private Path'}/fileman.dat" or return $i->error('WRITEOPEN', 'FATAL', $i->{config}->{'Private Path'} . '/fileman.dat', $!);
            print {$fh} "$admin_line\n";
            close $fh;
            chmod(0666,"$i->{config}->{'Private Path'}/fileman.log");
            open $fh, ">$i->{config}->{'Private Path'}/fileman.log" or return $i->error('WRITEOPEN', 'FATAL', $i->{config}->{'Private Path'} . '/fileman.log', $!);
            close $fh;
        }
        elsif ($i->{config}->{fversion} eq 'single') {
            $cfg{permission}    = { chmod => '1', command => '1', compress => '1', copy => '1', delete => '1', diff => '1', download => '1', edit => '1', makedir => '1', move => '1', newfile => '1', perl => '1', preferences => '1', protect => '1', rename => '1', replace => '1', search => '1', symlink => '1', tail => '1', upload => '1'};
            $cfg{allowed_space} = '0';
            $cfg{login}         = { username => $i->{config}->{'Username'}, password => encrypt($i->{config}->{'Password'}) };
            $cfg{default}       = { upload_mode => '644' };
        }

        my $fh = \do{ local *FH; *FH };
        open $fh, ">$path" or return $i->error('WRITEOPEN', 'FATAL', $path, $!);
        print {$fh} GT::Dumper->dump(var => '', data => \%cfg);
        close $fh;
        chmod(0666, $path);

# Create .htaccess
        my $htaccess = qq!
AuthGroupFile  /dev/null
AuthName       Protected
AuthType Basic
deny from all
!;
        open FILE, ">$cfg{private_path}/.htaccess" or return $i->error('WRITEOPEN', 'FATAL', "$cfg{private_path}/.htaccess", $!);
        print FILE $htaccess;
        close FILE;

        update_taint($cfg{cgi_path});
    }
    else { # We are doing an upgrade so we need to merge the new hash
        my $old_path = $i->{config}->{fversion} eq 'multiple' ? $i->{config}->{'Private Path'} . '/lib/FileMan/ConfigData.pm' : $i->{config}->{'Private Path'} . '/ConfigData.pm';
        $old_path    = $i->{config}->{'Private Path'} . '/fileman.cfg' unless $old_path;
        if (-e $old_path) {
            my $old_cfg  = do $old_path;
            ($@ || $!) and return $i->error($INST::LANG{error_configfile}."$old_path $! $@", 'FATAL');

            my $old_ver = join "_", $old_cfg->{version} =~ /(\d+)\.(\d+)\.(\d+)/;
            my $safety  = 0;

            while (my $next_ver = $VERSION_TREE{$old_ver}) { # Walk the version upgrade tree
                my $func = "upgrade__${old_ver}__${next_ver}";
                if (defined &$func) {
                    no strict 'refs';
                    $func->($i, $old_cfg, $old_path) or return;
                }
                if ($safety++ > 100) {
                    return $i->error("Invalid upgrade path. Couldn't upgrade from $old_ver => $next_ver", 'FATAL');
                }
                $old_ver = $next_ver;
            }
        }
        else {
            my $cfg  = do $path;
            ($@ || $!) and return $i->error($INST::LANG{error_configfile}."$path $! $@", 'FATAL');
            if ($i->{config}->{fversion} eq 'multiple' and $i->{config}->{fversion} ne $cfg->{fversion}) { # We are doing an upgrade from single user version to multiple users version

                my $user = "1|$cfg->{login}->{username}|$cfg->{login}->{password}||email\@domain.com||||\n";
                open FILE, "> $i->{config}->{'Private Path'}/fileman.dat" or return $i->error('WRITEOPEN', 'FATAL', "$i->{config}->{'Private Path'}/fileman.dat", $!);
                print FILE $user;
                close FILE;
                chmod(0666,"$i->{config}->{'Private Path'}/fileman.dat");

                open FILE, "> $i->{config}->{'Private Path'}/fileman.log" or return $i->error('WRITEOPEN', 'FATAL', "$i->{config}->{'Private Path'}/fileman.log", $!);
                close FILE;
                chmod(0666,"$i->{config}->{'Private Path'}/fileman.log");

                $cfg->{reg_number}     = $i->{config}->{'Registration Number'};
                $cfg->{htpasswd_level} = '0';
                $cfg->{history_exp}    = '7';
                $cfg->{fversion}       = 'multiple';
                $cfg->{session}        = { cookie => 'session_fileman', expiry => '3' };
                $cfg->{default}        = { permission => \%DEFAULT_PERMISSION, allowed_space => '5000000' };
                $cfg->{email}          = { smtp_server => '', mail_path => '', admin => '' };
                $cfg->{filename_check} = $^O =~ /mswin/i ? 0 : 1;
            }

            $cfg->{version} = $i->{version};
            my $fh = \do{ local *FH; *FH };
            open $fh, ">$path" or return $i->error('WRITEOPEN', 'FATAL', $path, $!);
            print {$fh} GT::Dumper->dump(var => '', data => $cfg);
            close $fh;
        }
    }

    return 1;
}

sub update_taint {
    my $cgi_path = shift;

    return unless -f "$cgi_path/fileman.cgi";

    open (DATA, "< $cgi_path/fileman.cgi") or return;
    read DATA, my $content, -s DATA;
    close DATA;

    $content =~ s/\/perl/\/perl -T/ if $^O !~ /mswin/i;

    open (DATA, "> $cgi_path/fileman.cgi") or next;
    print DATA $content;
    close DATA;
}

sub encrypt {
#--------------------------------------------------------------------
# Encrypt password
#
    my ($clear_pass, $salt) = @_;
    $salt ||= join '', map +('a' .. 'z', 'A' .. 'Z', 0 .. 9, '.', '/')[rand 64], 1 .. 8;

    require GT::MD5;
    require GT::MD5::Crypt;
    return GT::MD5::Crypt::gt_md5_crypt($clear_pass, $salt);
}

sub upgrade__2_0_0__3_0_0 {
    my ($i, $old_cfg, $old_path) = @_;
    $i->print("\nPerforming FileMan 2.0.0 -> 3.0.0 upgrades...\n");
    _upgrade_3_0_0($i, $old_cfg, $old_path);
    $i->print("\nFileMan 2.0.0 -> 3.0.0 upgrades performed.\n", 'none');
}

sub upgrade__2_0_1__3_0_0 {
    my ($i, $old_cfg, $old_path) = @_;
    $i->print("\nPerforming FileMan 2.0.1 -> 3.0.0 upgrades...\n");
    _upgrade_3_0_0($i, $old_cfg, $old_path);
    $i->print("\nFileMan 2.0.1 -> 3.0.0 upgrades performed.\n", 'none');
}

sub upgrade__2_0_2__3_0_0 {
    my ($i, $old_cfg, $old_path) = @_;
    $i->print("\nPerforming FileMan 2.0.2 -> 3.0.0 upgrades...\n");
    _upgrade_3_0_0($i, $old_cfg, $old_path);
    $i->print("\nFileMan 2.0.2 -> 3.0.0 upgrades performed.\n", 'none');
}

sub upgrade__2_0_3__3_0_0 {
    my ($i, $old_cfg, $old_path) = @_;
    $i->print("\nPerforming FileMan 2.0.3 -> 3.0.0 upgrades...\n");
    _upgrade_3_0_0($i, $old_cfg, $old_path);
    $i->print("\nFileMan 2.0.3 -> 3.0.0 upgrades performed.\n", 'none');
}

sub upgrade__2_0_4__3_0_0 {
    my ($i, $old_cfg, $old_path) = @_;
    $i->print("\nPerforming FileMan 2.0.4 -> 3.0.0 upgrades...\n");
    _upgrade_3_0_0($i, $old_cfg, $old_path);
    $i->print("\nFileMan 2.0.4 -> 3.0.0 upgrades performed.\n", 'none');
}

sub upgrade__2_1_0__3_0_0 {
    my ($i, $old_cfg, $old_path) = @_;
    $i->print("\nPerforming FileMan 2.1.0 -> 3.0.0 upgrades...\n");
    _upgrade_3_0_0($i, $old_cfg, $old_path);
    $i->print("\nFileMan 2.1.0 -> 3.0.0 upgrades performed.\n", 'none');
}

sub upgrade__2_1_1__3_0_0 {
    my ($i, $old_cfg, $old_path) = @_;
    $i->print("\nPerforming FileMan 2.1.1 -> 3.0.0 upgrades...\n");
    _upgrade_3_0_0($i, $old_cfg, $old_path);
    $i->print("\nFileMan 2.1.1 -> 3.0.0 upgrades performed.\n", 'none');
}

sub _upgrade_3_0_0 {
    my ($i, $old_cfg, $old_path) = @_;

    my $path = $i->{config}->{'Private Path'} . '/fileman.conf';
    my %cfg;

    my ($image_dir) = $old_cfg->{image_path} =~ /\/([^\/]+)$/;
    my $static_path = $old_cfg->{image_path};
    my $static_url  = $old_cfg->{html_root_url};
    $static_path =~ s/\/$image_dir$//g;
    $static_url  =~ s/\/$image_dir$//g;

    $cfg{version}         = $i->{version};
    $cfg{path_to_perl}    = $old_cfg->{path_to_perl};
    $cfg{cgi_path}        = $old_cfg->{cgi_path}     || $old_cfg->{root_path};
    $cfg{cgi_url}         = $old_cfg->{cgi_url}      || "$old_cfg->{root_url}/fileman.cgi";
    $cfg{static_path}     = $old_cfg->{static_path}  || "$static_path/static";
    $cfg{static_url}      = $old_cfg->{static_url}   || "$static_url/static";
    $cfg{private_path}    = $old_cfg->{private_path} || $old_cfg->{'priv_path'};
    $cfg{root_path}       = $old_cfg->{root_dir};
    $cfg{filename_check}  = $old_cfg->{filename_check}   || 1;
    $cfg{debug}           = $old_cfg->{debug_level}      || 0;
    $cfg{command_timeout} = $old_cfg->{command_time_out} || 20;
    $cfg{template}        = 'luna';
    $cfg{session}         = { cookie => 'session_fileman', expiry => $old_cfg->{'session_exp'} || '3' };
    $cfg{date}            = { display => '%dd%-%mmm%-%yy% %hh%:%MM%:%ss%', input => '%yyyy%-%mm%-%dd%' };

    require GT::File::Tools;
    if ($i->{config}->{fversion} eq 'multiple') {
        $cfg{fversion}       = 'multiple';
        $cfg{reg_number}     = $old_cfg->{'reg_number'};
        $cfg{htpasswd_level} = $old_cfg->{passwd_dir_level} || 0;
        $cfg{history_exp}    = $old_cfg->{history_exp} || 7;
        $cfg{default}        = { permission => \%DEFAULT_PERMISSION, allowed_space => $old_cfg->{'default_diskspace'} || '5000000', upload_mode => $old_cfg->{upload_chmod} || '644' };
        $cfg{email}          = { smtp_server => $old_cfg->{db_smtp_server}, mail_path => $old_cfg->{db_mail_path}, admin => $old_cfg->{admin_email} };

# Rename fileman_data.db and fileman_history.db
        GT::File::Tools::move("$old_cfg->{priv_path}/fileman_data.db", "$cfg{private_path}/fileman.dat");
        GT::File::Tools::move("$old_cfg->{priv_path}/fileman_history.db", "$cfg{private_path}/fileman.log");
    }
    else {
        $cfg{fversion}       = 'single';
        $cfg{allowed_space}  = $old_cfg->{'allowed_space'} || '0';
        $cfg{permission}     = \%DEFAULT_PERMISSION;
        $cfg{login}          = { username => $old_cfg->{username}, password => $old_cfg->{password} };
        $cfg{default}        = { upload_mode => $old_cfg->{upload_chmod} || '644' };
    }
    my $fh = \do{ local *FH; *FH };
    open $fh, ">$path" or return $i->error('WRITEOPEN', 'FATAL', $path, $!);
    print {$fh} GT::Dumper->dump(var => '', data => \%cfg);
    close $fh;

# Remove old files
    GT::File::Tools::move("$old_cfg->{image_path}/static", $static_path);
    GT::File::Tools::move($old_cfg->{image_path}, "$old_cfg->{image_path}.old");

    GT::File::Tools::rmkdir("$old_cfg->{priv_path}/templates/old", 0755);
    GT::File::Tools::move("$old_cfg->{priv_path}/templates/*.html", "$old_cfg->{priv_path}/templates/old", { globbing => 1 });
    GT::File::Tools::move($old_path, "$old_path.bak");
}

sub telnet_callback {
# ---------------------------------------------------------------
# When the CGI path gets updated, update all the other paths to reflect this
# change.
#
    my $i = shift;
    my $cfg = $i->{config};
    my $def = $i->{defaults};

    my $cgi_path = $cfg->{'CGI Path'};
    if ($cgi_path !~ /cgi/i and $cfg->{'CGI URL'} !~ /cgi/i) {
        $def->{"Static Path"} = "$cgi_path/static";
        $def->{"Static URL"} = $cfg->{'CGI URL'} . '/static';
    }
    $def->{"Private Path"} = "$cgi_path/private";
    $def->{"Root Path"} = $cgi_path;

    return 1;
}
