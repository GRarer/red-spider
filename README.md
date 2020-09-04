# red-spider
Red Spider is a command-line tool that downloads webcomics for offline vieweing and archiving.

<img src="https://imgs.xkcd.com/comics/red_spiders_small.jpg" width="300">

## Usage

At minimum, you must specify the url of the first page of the comic.
For example, to download all panel images from the webcomic XKCD (skipping interactive pages that cannot be downloaded as images, such as https://xkcd.com/1416/) as `1.jpg`, `2.jpg`,
use the url of the first page:

```
red-spider --url "https://xkcd.com/1"
```

You can add a prefix to the file names, and have the file names start counting from a specific number.
This is useful if you are updating an archive of a comic you have already partially downloaded.
For example, to download xkcd starting on page 2000 with the file names `xkcd 2000.png`, `xkcd 2001.png`, etc.

```
red-spider --url "https://xkcd.com/2000/" --prefix "xkcd " -n 2000
```

By default, red spider identifies panel images by looking for the substring `/comics/` in the image url, and identifies the link to the next page using the `rel` attribute.
Some websites use different image url schemes or don't implement the `rel` hyperlink attribute, so you can use other matching mode options. For example:

```
red-spider --url "ohumanstar.com/comic/chapter-1-title-page/" --panelAlt "Chapter" --linkText "Next"
```


## Command-line options
```
red-spider --url URL [-p|--prefix PREFIX] [-n|--number N] [--panelSrc SUBSTRING | --panelAlt SUBSTRING] [--linkText SUBSTRING]
```
                  
- `--url URL`:               URL of first page

- `-p,--prefix PREFIX`:       Prefix label for output file names

- `-n,--number N`:            Number to label the first page (default: 1)

- `--panelSrc SUBSTRING`:     Select panel images that have a specific substring in their `src` url (default behavior is to select images with '\comics\' in their url
                         
- `--panelAlt SUBSTRING`:     Select panel images that have a specific substring in their `alt` attribute
                         
- `--linkText SUBSTRING`:     Locate link to next page by looking for a substring within the link's inner text (overrides default behavior of identifying links based on their `rel` attribute)
                         
- `-h,--help`:                Show help text
