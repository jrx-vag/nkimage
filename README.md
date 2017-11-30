# NkIMAGE

An image processing plugin for NetComposer

## API

There are two main functions offered by this plugin:

* `parse_processor` returns an image processor configuration
* `process` performs an actual image operation, such as format conversion, or thumbnail creation, using a processor.

## Sample processor configuration

An image processor describes what kind of underlying implementation or provider will be used to perform actual image conversion. As an example, `nkimage_imaginary` supports the following syntax:

```
Processor = #{ class => imaginary,
               config => #{ host => <<"...">>,
                            port => <<"...">>,
                            path => <<"...">>,
                            scheme => <<"...">>,
                            username => <<"...">>,
                            password => <<"...">> }}
```

`username` and `password` are required only for HTTP basic authentication.

## Supported image operations

* Image format conversion
* Image thumbnail generation

## Plugins

* `nkimage_imaginary`, based on an external HTTP based microservice. 
