package Stylish::REPL;
use Moose;
use 5.010;
# Stylish::REPL is Devel::REPL without a terminal

with qw/Devel::REPL::Plugin::LexEnv
        Devel::REPL::Plugin::DDS/;

has output => (
    is      => 'rw', # XXX
    isa     => 'CodeRef',
    default => sub { sub { warn 'NO OUTPUT CODEREF' } },
);

has error => (
    is      => 'rw', # XXX
    isa     => 'CodeRef',
    default => sub { sub { warn 'NO ERROR CODEREF' } },
);

sub wrap_as_sub {
  my ($self, $line) = @_;
  # the "use $]" is so that when running under 5.10, all the new 5.10
  # features become available
  return qq!use $]; sub {\n!.$self->mangle_line($line).qq!\n}\n!;
}

sub eval {
    my ($self, $line) = @_;
    my ($to_exec, @rest) = $self->compile($line);
    return @rest unless defined($to_exec);
    my @ret = $self->execute($to_exec);
    return @ret;
}

sub compile {
    my $_REPL = shift;
    my $compiled = eval $_REPL->wrap_as_sub($_[0]);
    return (undef, $_REPL->error_return("Compile error", $@)) if $@;
    return $compiled;
}

sub execute {
    my ($self, $to_exec, @args) = @_;
    my @ret = eval { $to_exec->(@args) };
    return $self->error_return("Runtime error", $@) if $@;
    return @ret;
}

sub error_return {
  my ($self, $type, $error) = @_;
  $self->error->($type, $error);
}

sub print {
    my ($self, @ret) = @_;
    $self->output->(@ret);
}

sub mangle_line {
    my ($self, $line) = @_;
    return $line;
}

1;
