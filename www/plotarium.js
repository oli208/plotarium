document.addEventListener('DOMContentLoaded', function() {
    const btn = document.getElementById('themeToggle');
    const html = document.documentElement;
    btn.addEventListener('click', function() {
      const current = html.dataset.theme;
      html.dataset.theme = (current === 'light') ? '' : 'light';
      btn.classList.toggle('light-mode');
    });
  });