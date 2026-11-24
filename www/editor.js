document.addEventListener('click', function(event) {
  if (!event.target.closest('.properties-toggle')) return;
  var workspace = event.target.closest('.statlab-workspace');
  if (workspace) workspace.classList.toggle('properties-collapsed');
  window.dispatchEvent(new Event('resize'));
});

document.addEventListener('change', function(event) {
  var el = event.target;
  if (!el.matches('.study-toggle')) return;
  Shiny.setInputValue(el.dataset.input, {id:el.dataset.study, field:el.dataset.field, checked:el.checked, nonce:Date.now()}, {priority:'event'});
});
document.addEventListener('click', function(event) {
  if (event.target.matches('.study-toggle')) event.stopPropagation();
}, true);

// Update checkbox states without rebuilding the table, search or row selection.
Shiny.addCustomMessageHandler('statlab-study-states', function(message) {
  var root = document.getElementById(message.output);
  if (!root) return;
  var node = $(root).find('table.dataTable').filter(function() { return this.tBodies.length > 0; }).first();
  if (!node.length || !$.fn.dataTable.isDataTable(node[0])) return;
  var table = node.DataTable();
  var lists = [message.active || [], message.visible || [], message.highlight || []];
  lists = lists.map(function(values) { return Array.isArray(values) ? values : [values]; });
  table.rows().every(function() {
    var row = this.data();
    lists.forEach(function(ids, index) {
      var holder = document.createElement('div'); holder.innerHTML = row[index + 2];
      var box = holder.querySelector('input');
      if (box) { box.checked = ids.indexOf(row[1]) !== -1; if (box.checked) box.setAttribute('checked','checked'); else box.removeAttribute('checked'); row[index + 2] = box.outerHTML; }
    });
    this.data(row);
  });
  table.draw(false);
});
