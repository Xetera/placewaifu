const $ = document.querySelector.bind(document);
const dest = $(".images");
var msnry = new Masonry(dest, {
  columnWidth: 200
});
fetch("/images")
  .then(res => res.json())
  .then(images => {
    const elems = images.map(
      image =>
        `
        <div class="card">
          <div class="card-image">
            <img src="data:image/png;base64, ${image.data}">
          </div>
          <div class="card-content">
            <p class="bold">${image.name}</p>
            <a class="is-6" href="${image.source}">To source</a>
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
