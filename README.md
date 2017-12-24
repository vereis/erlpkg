# erlpkg
Erlpkg is a small, simple and minimal Erlang utility to create standalone escripts for your Erlang projects.
This was originally written for [Jarlang](https://github.com/vereis/jarlang), which required other filetypes to be added into a resultant escript package, and thus ```erlpkg``` has the neat ability to encode arbitary filetypes and include them in generated escripts.
The main goal of ```erlpkg``` is ultimately to require no dependencies, and to allow you to build escripts containing anything you want as easily as running the erlpkg ```escript``` followed by a list of file arguments.

## Getting Started
Note that this project is still in development and very likely contains overlooked bugs and issues. Please be weary of this when considering this for real world usage.

### Requirements
- Erlang 18 (I assume other versions will work just fine, however, I've only tested this on a machine with Erlang 18 installed)

### Installing
1) Clone the repo with ```git clone https://github.com/vereis/erlpkg``` in your command line. This should create a folder in the working directory called ```erlpkg```.
2) Enter the ```erlpkg``` directory with ```cd erlpkg```
3) You can use the pre-packaged binary in the ```bin``` directory or you can package your own version of ```erlpkg``` by opening up the Erlang shell and running the commands:
```erlang
cd(src).
c(erlpkg).
erlpkg:build(["erlpkg", "erlpkg.erl"]).
```

Note: Any type of file can be included in an escript as escripts are essentially just fancy zip files, right now theres no way to easily and cleanly utilise other files added into an escript but one can easily play around with them by manually unzipping the contents of the escript. Additional erlpkg modules are going to be written and included in generated escripts by default for this purpose.

### Automatic Testing
To be added

## Contribution Guidelines
Please ensure automatic testing passes, when implemented, before pushing any commits. Also ensure you don't break the build.
