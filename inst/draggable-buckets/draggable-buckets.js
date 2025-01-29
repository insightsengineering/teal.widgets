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
      event.target.classList.contains("elements")) &&
    areSameWidget(document.getElementById(data), event.target)
  )
    event.target.appendChild(document.getElementById(data));
};

const dropReorder = (event) => {
  event.preventDefault();
  const data = event.dataTransfer.getData("element");
  if (
    data !== null &&
    event.target.classList.contains("element") &&
    areSameWidget(document.getElementById(data), event.target)
  )
    event.target.parentNode.insertBefore(
      document.getElementById(data),
      document.getElementById(event.target.id)
    );
};

const dropBucketName = (event) => {
  event.preventDefault();
  const data = event.dataTransfer.getData("element");
  if (
    data !== null &&
    event.target.classList.contains("bucket-name") &&
    areSameWidget(document.getElementById(data), event.target)
  ) {
    const thirdChild = event.target.nextSibling.nextSibling;
    event.target.parentNode.insertBefore(
      document.getElementById(data),
      thirdChild
    );
  }
};

const areSameWidget = (object1, object2) => {
  return object1.dataset.widget === object2.dataset.widget;
};

// Shiny callbacks
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
        .filter(
          (child) =>
            child.classList !== undefined && child.classList.contains("element")
        )
        .map((node) => node.textContent);
      ret[bucket.dataset.label] = items;
    });
    return ret;
  },
  setValue: function (el, value) {
    $(el).text(value);
  },

  // The following two methods, setInvalid and clearInvalid, will be called
  // whenever this input fails or passes (respectively) validation.
  setInvalid: function (el, data) {
    el.setAttribute("data-error-message", data.message);
    el.classList.add("draggableBuckets-invalid");
  },
  clearInvalid: function (el) {
    el.removeAttribute("data-error-message");
    el.classList.remove("draggableBuckets-invalid");
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
