HTMLWidget <- R6::R6Class("HTMLWidget",
                             public = list(
                                 initialize = function() {
                                     # Inizializzazione della classe, non ha argomenti particolari.
                                 },

                                 generate_message = function(type = "info", message = "", animate_icon = TRUE) {
                                     # Definisci l'HTML per diversi tipi di messaggio: info, warning, error
                                     icon <- ""
                                     style <- ""

                                     animate_tag <- if (animate_icon) "<animate attributeName='r' values='20;25;20' dur='1.2s' repeatCount='3' />" else ""

                                     if (type == "info") {
                                         icon <- sprintf(
                                             "<svg width='50' height='50' xmlns='http://www.w3.org/2000/svg'>
                        <circle cx='25' cy='25' r='20' fill='#3e6da9'>%s</circle>
                        <text x='25' y='29' dominant-baseline='middle' text-anchor='middle' fill='white' font-size='30' font-family='Arial Black'>i</text>
                    </svg>",
                                             animate_tag
                                         )
                                         style <- "border-left: 5px solid #3e6da9; background-color: #f0f4fb; color: #3e6da9;"
                                     } else if (type == "warning") {
                                         icon <- sprintf(
                                             "<svg width='50' height='50' xmlns='http://www.w3.org/2000/svg'>
                        <circle cx='25' cy='25' r='20' fill='orange'>%s</circle>
                        <text x='25' y='29' dominant-baseline='middle' text-anchor='middle' fill='white' font-size='30' font-family='Arial Black'>!</text>
                    </svg>",
                                             animate_tag
                                         )
                                         style <- "border-left: 5px solid orange; background-color: #fff4e5; color: orange;"
                                     } else if (type == "error") {
                                         icon <- sprintf(
                                             "<svg width='50' height='50' xmlns='http://www.w3.org/2000/svg'>
                        <circle cx='25' cy='25' r='20' fill='red'>%s</circle>
                        <text x='25' y='29' dominant-baseline='middle' text-anchor='middle' fill='white' font-size='25' font-family='Arial Black'>X</text>
                    </svg>",
                                             animate_tag
                                         )
                                         style <- "border-left: 5px solid red; background-color: #fdecea; color: red;"
                                     }

                                     # Genera il contenuto HTML
                                     html_content <- sprintf(
                                         "<div style='display: flex; align-items: center; padding: 15px; margin: 10px; border-radius: 10px; %s'>
                    <div style='margin-right: 15px;'>%s</div>
                    <div>
                        <div style='font-family: Arial, sans-serif; font-size: 18px; font-weight: bold; margin-bottom: 5px;'>%s</div>
                        <div style='font-family: Arial, sans-serif; font-size: 16px;'>%s</div>
                    </div>
                </div>",
                                         style, icon, toupper(type), message
                                     )

                                     return(html_content)
                                 },
                                 # Funzione per generare un accordion HTML
                                 generate_accordion = function(title = "Help", content = "") {
                                     html_content <- paste0(
                                         '<style>',
                                         '.accordion {',
                                         '  background-color: #3498db;',
                                         '  color: white;',
                                         '  cursor: pointer;',
                                         '  padding: 8px 15px;',
                                         '  width: 100%;',
                                         '  border: none;',
                                         '  text-align: left;',
                                         '  outline: none;',
                                         '  font-size: 16px;',
                                         '  transition: 0.4s;',
                                         '  display: flex;',
                                         '  align-items: center;',
                                         '  position: relative;',
                                         '  border-top-left-radius: 8px;',
                                         '  border-top-right-radius: 8px;',
                                         '}',
                                         '.accordion svg {',
                                         '  margin-right: 15px;',
                                         '  transition: fill 0.4s;',
                                         '}',
                                         '.accordion svg .circle {',
                                         '  fill: white;',
                                         '}',
                                         '.accordion svg .horizontal,',
                                         '.accordion svg .vertical {',
                                         '  fill: #3498db;',
                                         '  transition: transform 0.8s ease-in-out;',
                                         '  transform-origin: center;',
                                         '}',
                                         '.accordion.active svg .vertical {',
                                         '  transform: scaleY(0);',
                                         '}',
                                         '.panel {',
                                         '  padding: 0 15px;',
                                         '  display: none;',
                                         '  background-color: white;',
                                         '  overflow: hidden;',
                                         '}',
                                         '</style>',
                                         '<script>',
                                         'var acc = document.getElementsByClassName("accordion");',
                                         'for (var i = 0; i < acc.length; i++) {',
                                         '  acc[i].addEventListener("click", function() {',
                                         '    this.classList.toggle("active");',
                                         '    var panel = this.nextElementSibling;',
                                         '    if (panel.style.display === "block") {',
                                         '      panel.style.display = "none";',
                                         '    } else {',
                                         '      panel.style.display = "block";',
                                         '    }',
                                         '  });',
                                         '}',
                                         '</script>',
                                         '<button class="accordion" style="margin-top: 20px;">',
                                         '  <svg width="20" height="18" viewBox="0 0 24 24">',
                                         '    <circle class="circle" cx="12" cy="12" r="11" />',
                                         '    <rect class="horizontal" x="5" y="11" width="15" height="3" />',
                                         '    <rect class="vertical" x="11" y="5" width="3" height="15" />',
                                         '  </svg>',
                                         '  <span style="font-size: 16px;">', title, '</span>',
                                         '</button>',
                                         '<div class="panel">',
                                         content,
                                         '</div>'
                                     )
                                     return(html_content)
                                 }

                             )
)

