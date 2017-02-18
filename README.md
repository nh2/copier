# copier - copies files in parallel for use on networked file systems

## Usage example

`copier --jobs 24 /path/to/sourcedir /path/to/destdir`

## Building

`stack build`. Then `stack exec -- which copier` shows you the path to the built binary.
