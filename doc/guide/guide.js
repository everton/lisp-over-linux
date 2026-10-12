// guide.js — the one behaviour every guide page shares: the theme button.
// auto -> light -> dark -> auto. The choice is a per-browser convenience kept in
// localStorage, wrapped in try/catch because file:// pages may have no storage.
(function () {
  var order = ["auto", "light", "dark"];
  function get() { try { return localStorage.getItem("lol-guide-theme") || "auto"; } catch (e) { return "auto"; } }
  function set(v) { try { localStorage.setItem("lol-guide-theme", v); } catch (e) {} }
  function apply(v) {
    if (v === "auto") document.documentElement.removeAttribute("data-theme");
    else document.documentElement.setAttribute("data-theme", v);
    var b = document.querySelector("[data-theme-toggle]");
    if (b) b.textContent = v;
  }
  apply(get());
  document.addEventListener("DOMContentLoaded", function () {
    apply(get());
    var b = document.querySelector("[data-theme-toggle]");
    if (b) b.addEventListener("click", function () {
      var v = order[(order.indexOf(get()) + 1) % order.length];
      set(v); apply(v);
    });
  });
})();
