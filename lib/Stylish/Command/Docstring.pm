package Stylish::Command::Docstring;
use Moose::Role;
use Class::MOP;
use feature 'switch';

sub command_docstring {
    my ($self, $client, @args) = @_;
    my $docstring = $self->_get_docstring(@args);
    return [
        docstring => \$docstring,
    ];
}

sub _get_docstring {
    my ($self, $package, $type, $argument) = @_;

    my $doc = sub {
        given($type){
            when('attribute'){
                eval { Class::MOP::load_class($package) };
                my $meta = eval { $package->meta };
                
                if($meta){
                    my $attr = $meta->get_attribute($argument);
                    if($attr){
                        my $doc = $attr->documentation;
                        return $doc if $doc;
                        return "No documentation found for ${package}::$argument";
                    }
                    return "No attribute $argument in $package";
                }
                return "No class $package!";
            }
        }
    }->();

    return $doc;
}

1;
