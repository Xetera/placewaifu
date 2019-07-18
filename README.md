# Placewaifu

<img src="assets/banner.jpg" width="500px">

Placewaifu allows users to generate anime placeholder images of any size.

## Usage

Just place an image tag with the following endpoints, something like this

```html
<img src="https://placewaifu.com/image/200" />
```

## Endpoints

- `/image` - Random placeholder of any size from the database
- `/image/:width` - Square placeholder of `:width` length
- `/image/:width/:height`- Resized placeholder of`:width` x `:height` dimensions
- `/images` - List of all image data

### Filter options

- `greyscale` - black and white filter
- `blur` - blur the image

##### Example

```
/image/200/500?greyscale&blur
```

Valid for every endpoint

All images are served as svgs for fast resizing

## Contributing

Feel free to PR any new images you'd like to see get served.

NSFW content will **not** be accepted.

Images that are not contributed by PRs are taken from [hifumi](https://github.com/moedevs/hifumi)'s database.

## If you're interested in adding new features please submit an issue first.

Heavily inspired by [placekeanu](https://github.com/alexandersandberg/placekeanu.com)
