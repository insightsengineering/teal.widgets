const allowDrop = (event) => {
  event.preventDefault();
};

const drag = (event) => {
  event.dataTransfer.setData("element", event.target.id);
};

const drop = (event) => {
  event.preventDefault();
  const data = event.dataTransfer.getData("element");
  if (
    data !== null &&
    (event.target.classList.contains("bucket") ||
      event.target.classList.contains("elements"))
  )
    event.target.appendChild(document.getElementById(data));
};

const drop_end = (event) => {
  event.preventDefault();
  const data = event.dataTransfer.getData("element");
  if (data !== null && event.target.classList.contains("element"))
    event.target.parentNode.appendChild(document.getElementById(data));
};

const drop_reorder = (event) => {
  event.preventDefault();
  const data = event.dataTransfer.getData("element");
  if (data !== null && event.target.classList.contains("element"))
    event.target.parentNode.insertBefore(
      document.getElementById(data),
      document.getElementById(event.target.id))
    ;
};

const drop_bucket_name = (event) => {
  event.preventDefault();
  const data = event.dataTransfer.getData("element");
  console.log("sdfs", event.target.parentNode.childNodes, event.target.parentNode)
  if (data !== null && event.target.classList.contains("bucket-name")) {
    // needs to be 3 because the name div is surrounded by 2 text div's
    // so draggable elements starts at index 3
    if (event.target.parentNode.childNodes.length > 3) {
      event.target.parentNode.insertBefore(
        document.getElementById(data),
        document.getElementById(event.target.parentNode.childNodes[3].id)
      );
    } else {
      event.target.parentNode.appendChild(document.getElementById(data));
    }
  }
};

var draggableBuckets = new Shiny.InputBinding();
$.extend(draggableBuckets, {
  find: function (scope) {
    return $(scope).find(".draggableBuckets");
  },
  getValue: function (el) {
    const buckets = $(el).find(".bucket").toArray();
    const ret = {};
    buckets.forEach((bucket, index) => {
      const items = [...bucket.childNodes]
        .filter((child) => child.classList !== undefined && child.classList.contains("element"))
        .map((node) => node.textContent);
      ret[index] = {
        name: bucket.dataset.label,
        elements: items,
      };
    });
    return ret;
  },
  setValue: function (el, value) {
    $(el).text(value);
  },
  subscribe: function (el, callback) {
    if (this.observers === undefined) this.observers = {};
    this.observers[el] = new MutationObserver(callback);
    this.observers[el].observe(el, {
      subtree: true,
      childList: true,
      attributes: true,
    });
  },
  unsubscribe: function (el) {
    this.observers[el].disconnect();
  },
});

Shiny.inputBindings.register(draggableBuckets);
