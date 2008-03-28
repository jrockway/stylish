package Stylish::Syntax::Control;
use Moose;
use MooseX::AttributeHelpers;
use Moose::Util::TypeConstraints;

subtype KeywordType => as 'Int';

has 'keywords' => (
    metaclass => 'Collection::ImmutableHash',
    isa       => 'HashRef[KeywordType]',
    default   => sub {
        +{
            use     => 1,
            require => 1,
            if      => 1,
            # and lots more
        },
    },
    provides => {
        exists => 'is_keyword',
        get    => 'keyword_type',
    }
);

1;
