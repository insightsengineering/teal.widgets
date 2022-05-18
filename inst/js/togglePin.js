const togglePin = (event) => {
  event.preventDefault();

  event.target.parentElement.classList.toggle("pin--active");
  event.target.parentElement.classList.toggle("pin--inactive");
  event.target.closest("#output-container").classList.toggle("pinned");
}
