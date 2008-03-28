package Stylish::Format::Sexp;
use Moose;
use feature ':5.10';

use Data::SExpression;

with 'Stylish::Format';

has 'parser' => (
    is  => 'ro',
    isa => 'Data::SExpression',
    default => sub {
        return Data::SExpression->new({
            fold_dashes      => 1,
            use_symbol_class => 1,
            symbol_case      => 'down',
        });
    },
);

sub format {
    my ($self, $item) = @_;
    return $item unless ref $item;
    return '('. (join ' ', map { $self->format($_) } @$item) .')';
}

sub parse {
    my ($self, $exp) = @_;
    return $self->parser->read($exp);
}

1;
