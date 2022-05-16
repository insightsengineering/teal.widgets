const allowDrop = (event) => {
  event.preventDefault();
};

const drag = (event) => {
  event.dataTransfer.setData("element", event.target.id);
};

const drop = (event) => {
  event.preventDefault();
  const data = event.dataTransfer.getData("element");
  if (data !== null) event.target.appendChild(document.getElementById(data));
};

var draggableBuckets = new Shiny.InputBinding();
$.extend(draggableBuckets, {
  find: function (scope) {
    return $(scope).find(".draggableBuckets");
  },
  getValue: function (el) {
    const buckets = $(el).find(".bucket").toArray();
    return buckets.map((bucket) => {
      const items = [...bucket.childNodes].map(node => node.textContent);
      console.log(items);
      return {
        name: bucket.dataset.label,
        elements: items,
      };
    });
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
      attributes: true
    });
  },
  unsubscribe: function (el) {
    this.observers[el].disconnect();
  },
});

Shiny.inputBindings.register(draggableBuckets);
