use MooseX::Declare;

class Stylish::Plugin::LoadPlugin with Stylish::Plugin {
    method do_request(Str :$plugin){
        my $code = $self->server->load_plugin( $plugin );
        return { id => 0+$code };
    }
}
