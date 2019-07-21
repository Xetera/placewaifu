const $ = document.querySelector.bind(document);
const dest = $(".images");
var msnry = new Masonry(dest, {
  columnWidth: 200
});
fetch("/images")
  .then(res => res.json())
  .then(images => {
    const elems = images.sort((x, y) => x.name.localeCompare(y.name)).map(
      image =>
        `
        <div class="card">
          <div class="card-image">
            <img class="image" src="data:image/${image.format};base64, ${image.data}">
          </div>
          <div class="card-content">
            <p class="float-left">${image.name}</p>
            <a class="float-right is-6" href="${image.source}">source</a>
          </div>
        </div>
      `
    );
    dest.innerHTML = elems.join("\n");
  })
  .catch(err => {
    dest.innerText = `Something went wrong...`;
    console.log(err);
  });
