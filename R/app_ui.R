app_ui <- function() {
  shiny::fluidPage(
    title = "Structured Expert Elicitation-AMCHSS",
    shiny::tags$head(
      shiny::tags$meta(name = "viewport", content = "width=device-width, initial-scale=1, shrink-to-fit=no"),
      shiny::tags$link(rel = "stylesheet", type = "text/css", href = "styles.css"),
      shiny::tags$link(rel = "preconnect", href = "https://fonts.googleapis.com"),
      shiny::tags$link(rel = "preconnect", href = "https://fonts.gstatic.com", crossorigin = NA),
      shiny::tags$link(
        rel = "stylesheet",
        href = "https://fonts.googleapis.com/css2?family=Inter:wght@400;500;600;700&family=Plus+Jakarta+Sans:wght@500;600;700;800&display=swap"
      ),
      shiny::tags$script(shiny::HTML(
        "document.addEventListener('shiny:connected', function() {
          var lastSent = 0;
          function activity() {
            var now = Date.now();
            if (now - lastSent > 30000) {
              lastSent = now;
              Shiny.setInputValue('ee_activity', now, {priority: 'event'});
            }
          }
          ['click', 'keydown', 'mousemove', 'touchstart'].forEach(function(name) {
            document.addEventListener(name, activity, {passive: true});
          });
          activity();
        });
        Shiny.addCustomMessageHandler('ee_oidc_redirect', function(url) {
          window.location.assign(url);
        });
        window.eeCopyLink = function(text, btn) {
          function showDone() {
            if (btn) {
              var orig = btn.innerText;
              btn.innerText = 'Copied!';
              setTimeout(function() { btn.innerText = orig; }, 2000);
            }
          }
          if (navigator.clipboard && window.isSecureContext) {
            navigator.clipboard.writeText(text).then(showDone).catch(function() {
              fallback(text, showDone);
            });
          } else {
            fallback(text, showDone);
          }
          function fallback(val, cb) {
            var ta = document.createElement('textarea');
            ta.value = val;
            ta.style.position = 'fixed';
            ta.style.opacity = '0';
            document.body.appendChild(ta);
            ta.focus();
            ta.select();
            try { document.execCommand('copy'); cb(); } catch(e) {}
            document.body.removeChild(ta);
          }
        };"
      ))
    ),
    shiny::uiOutput("root")
  )
}
