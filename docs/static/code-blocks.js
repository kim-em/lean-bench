// Tag <pre> blocks whose first line starts with `$ ` (the synthetic
// shell prompt that the bench / tempProjRun directives prepend) with
// the `terminal` class, so the stylesheet can render them as a
// dark-background terminal session. Decoupled from Verso's block
// pipeline — server-side, those blocks render as plain <pre>; this
// adds the visual differentiation purely in the browser.

(() => {
  const classify = () => {
    document.querySelectorAll('pre').forEach((pre) => {
      // Skip Verso's syntax-highlighted Lean blocks; they're never
      // terminal output and they have their own coloring.
      if (pre.classList.contains('hl') || pre.querySelector('.hl')) return;

      const first = (pre.textContent || '').split('\n', 1)[0] || '';
      if (first.startsWith('$ ') || first === '$') {
        pre.classList.add('terminal');
        // Wrap the prompt line in its own span so CSS can colour the
        // command differently from the output below.
        const text = pre.textContent;
        const nl = text.indexOf('\n');
        if (nl > -1) {
          const cmd = text.slice(0, nl);
          const rest = text.slice(nl);
          pre.innerHTML = '';
          const span = document.createElement('span');
          span.className = 'prompt-line';
          span.textContent = cmd;
          pre.appendChild(span);
          pre.appendChild(document.createTextNode(rest));
        }
      }
    });
  };
  if (document.readyState === 'loading') {
    document.addEventListener('DOMContentLoaded', classify);
  } else {
    classify();
  }
})();
