package Stylish::Command::REPL;
use Moose::Role;

use Stylish::REPL;
use File::Slurp qw(read_file);
use IO::CaptureOutput qw(capture);

# TODO: per-client?
has 'REPL' => (
    is      => 'ro',
    isa     => 'Stylish::REPL',
    default => sub {
        Stylish::REPL->new;
    },
);

sub command_repl {
    my ($self, $client, $string) = @_;

    # handlers for output/error
    my $status;
    my $result;
    my $get_output = sub { 
        $result = join '', @_;
        $status = 'success';
    };
    my $get_error = sub {
        $result = join ': ', @_; # XXX
        $status = 'error';
    };
      
    # tell them about us
    $self->REPL->output($get_output);
    $self->REPL->error($get_error);

    # eval the request (and format if there are no errors)
    my ($stdout, $stderr);
    capture {
        my @result = $self->REPL->eval($string);
        $self->REPL->print(@result) unless $status;
    } \$stdout, \$stderr;
    
    return [
        repl => 
          ":$status", \$result,
        ':stdout', \$stdout,
        ':stderr', \$stderr,
    ];
            
}

sub command_repl_load_file {
    my ($self, $client, $file) = @_;

    # read the file in manually so we can add a "1;" at the end
    my $file_name = quotemeta $file;
    my $text = eval { read_file $file } || qq{die "Could not read $file_name"};
    return $self->command_repl($client, qq{$text ;"Loaded $file_name OK";});
}
1;
