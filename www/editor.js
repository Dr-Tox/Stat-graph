document.addEventListener('click', function(event) {
  if (!event.target.closest('.properties-toggle')) return;
  var workspace = event.target.closest('.statlab-workspace');
  if (workspace) workspace.classList.toggle('properties-collapsed');
  window.dispatchEvent(new Event('resize'));
});
