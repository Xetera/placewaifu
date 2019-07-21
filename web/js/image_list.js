const $ = document.querySelector.bind(document);
const dest = $(".images");
fetch("/images")
  .then(res => res.json())
  .then(images => {
    const elems = images.sort((x, y) => x.name.localeCompare(y.name)).map(
      image =>
        `<div class="grid-item">
            <img class="image" src="data:image/${image.format};base64, ${image.data}">
            <div class="image-info">
              <div class="image-name">
                ${image.name}
              </div>
              <a class="image-source" href="${image.source}" target="_blank" rel="noopener noreferrer">
                Source
              </a>
            </div>
        </div>`
    );
    dest.innerHTML = elems.join("\n");
    imagesLoaded(dest, () => {
      const msnry = new Isotope(dest, {
        itemSelector: ".grid-item",
        masonry: {
          columnWidth: 300
        }
      });
    })
  })
  .catch(err => {
    dest.innerText = `Something went wrong...`;
    console.log(err);
  });
