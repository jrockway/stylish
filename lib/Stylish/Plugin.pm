use MooseX::Declare;

role Stylish::Plugin {
    has 'server' => (
        is       => 'ro',
        isa      => 'Stylish::Server',
        required => 1,
    );

    requires 'do_request';
}
