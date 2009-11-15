use MooseX::Declare;

class Stylish::Plugin::InPackage with Stylish::Plugin {
    use Devel::InPackage qw(in_package);

    method do_request(Str :$file, Int :$line){
        return { package => in_package( file => $file, line => $line ) };
    }
}
