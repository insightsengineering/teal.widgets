// this code alows the show R code "copy to clipbaord" button to work
const copyToClipboard = (elementIdToCopy, onFailure = () => null) => {
  const copiedContent = document.getElementById(elementIdToCopy).innerText;
  if (navigator.clipboard) {
    navigator.clipboard.writeText(copiedContent).then(() => {
      let range = document.createRange();
      range.selectNodeContents(document.getElementById(elementIdToCopy));
      let selected = window.getSelection();
      selected.removeAllRanges();
      selected.addRange(range);
    }, onFailure);
    return;
  }

  // Inspired by this SO post:
  // https://stackoverflow.com/questions/400212/how-do-i-copy-to-the-clipboard-in-javascript/30810322#30810322
  let textArea = document.createElement("textarea");
  textArea.value = copiedContent;
  textArea.contentEditable = true;

  // Avoid scrolling to bottom
  textArea.style.top = "0";
  textArea.style.left = "0";
  textArea.style.position = "fixed";

  document.body.appendChild(textArea);
  textArea.focus();
  textArea.select();

  try {
    document.execCommand("copy");
  } catch (err) {
    console.error("Unable to copy the content to the clipboard", err);
  }

  document.body.removeChild(textArea);
};

// Primarily added to make sure the user does not open multiple modals when shiny is busy.
// https://github.com/rstudio/shiny/issues/4261
$(document).on("shiny:busy", function () {
  $(".teal-widgets-busy-disable").prop("disabled", true);
});
$(document).on("shiny:idle", function () {
  $(".teal-widgets-busy-disable").prop("disabled", false);
});
