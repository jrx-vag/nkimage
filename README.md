# NkIMAGE

An image processing plugin for NetComposer

## Supported conversions

* JPEG to PNG
* PNG to JPEG
* TIFF to PNG, JPEG (only first page)
* TIFF to PNG (multipage)

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

## Sample image thumbnail request

Creating a thumbnail is as easy as doing:


```
Req => #{ action => resize,
          from => <<"image/jpeg">>,
          to => <<"image/png">>,
          width => 150,
          height => 100,
          body => FileData }).

{ok, ThumbnailData} = nkimage:process(SrvId, Processor, Req).

```

where `FileData` are the byte data of the original image, and `ThumbnailData` are the bytes of the resulting thumbnail image. 


## Supported image operations

* Image format conversion:

```
Req => #{ action => convert,
          from => <<"image/jpeg">>,
          to => <<"image/png">>,
          body => FileData }).
```

* Image thumbnail generation:


```
Req => #{ action => resize,
          from => <<"image/jpeg">>,
          to => <<"image/png">>,
          width => 150,
          height => 100,
          body => FileData }).
```

## Plugins

* `nkimage_imaginary`, based on an external HTTP based microservice. 
