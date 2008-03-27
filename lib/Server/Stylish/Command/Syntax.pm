package Server::Stylish::Command::Syntax;
use Moose::Role;

use Server::Stylish::Syntax::PPI;

sub command_highlight {
    my ($self, $client, $tag, $offset, $code) = @_;
    my $doc = Server::Stylish::Syntax::PPI->new(document => $code);
    return [ 
        highlight => [
            [ tag    => $tag ],
            [ result => $doc->syntax ],
        ],
    ];
}

1;
