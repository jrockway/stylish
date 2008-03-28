package Stylish::Command::Syntax;
use Moose::Role;

use Stylish::Syntax::PPI;

sub command_highlight {
    my ($self, $client, $tag, $offset, $code) = @_;
    my $doc = Stylish::Syntax::PPI->new(document => $code, offset => $offset);
    return [ 
        highlight => [
            [ tag    => $tag ],
            [ result => $doc->syntax ],
        ],
    ];
}

1;
