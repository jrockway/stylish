package Stylish::Syntax::PPI;
use feature ':5.10';
use Moose;
use Moose::Util::TypeConstraints;
use PPI;

use Stylish::Syntax::Control;

subtype 'PPI::Document',
  as 'Class',
  where { $_->isa('PPI::Document') };

coerce 'PPI::Document',
  from 'Str',
  via { PPI::Document->new(\$_, readonly => 1) };

subtype 'Offset',
  as 'Int',
  where { $_ >= 0 };

has 'document' => (
    is       => 'ro',
    isa      => 'PPI::Document',
    coerce   => 1,
    required => 1,
);

has 'syntax_controller' => (
    is      => 'ro',
    isa     => 'Stylish::Syntax::Control',
    default => sub {
        Stylish::Syntax::Control->new,
      }
);

has 'offset' => (
    is       => 'ro',
    isa      => 'Offset',
    default  => 1, # emacs convention
    required => 1,
);

has 'syntax' => (
    is      => 'ro',
    isa     => 'ArrayRef',
    lazy    => 1,
    builder => '_build_syntax',
);

# returns list of [ [start, end], type ] cells

sub _se($){ 
    return [
        $_[0]->{start} // confess('no start'), 
        $_[0]->{end}   // confess('no end'),
    ]; 
}

sub analyze {
    my ($self, $tree) = @_;

    my @result;
    foreach my $node ($tree->children){
        if($node->isa('PPI::Statement::Include')){
            my $keyword = $node->schild(0);
            my $module  = $node->schild(1);
            push @result,
              [ _se($keyword), 'keyword' ],
              [ _se($module),  'module-name'],
          }
    }
    
    return @result;
}

sub _build_syntax {
    my $self = shift;
    my $document = $self->document;
    
    # add offset information to DOM nodes
    my @nodes = _linearize($document);
    _compute_offsets($self->offset, @nodes);

    # analyze syntax
    return [$self->analyze($document)];
}

sub _linearize {
    my $node = shift;

    return $node unless $node->can('elements');

    my @result;
    foreach my $element ($node->elements){
        push @result, _linearize($element);
    }
    return @result;
}

# this fucks with the original document.  nice, eh

sub _compute_offsets {
    my $start = shift;
    for my $node (@_){
        $node->{start} = $start;
        $start = $node->{end} = $start + length $node->content;
    }
}

__PACKAGE__->meta->make_immutable;

1;
