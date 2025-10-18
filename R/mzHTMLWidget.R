# ---- put this near the top of b.R (before it's used) ----

# minimal HTML escaper (to avoid htmltools dependency)
.html_escape <- function(x) {
  x <- gsub("&", "&amp;", x, fixed = TRUE)
  x <- gsub("<", "&lt;",  x, fixed = TRUE)
  x <- gsub(">", "&gt;",  x, fixed = TRUE)
  x
}

HTMLWidget <- R6::R6Class("HTMLWidget",
                          public = list(
                            initialize = function() {},
                            
                            # (옵션) 메시지 카드가 필요없다면 이 메서드는 삭제해도 됩니다.
                            generate_message = function(type = "info", message = "") {
                              style <- switch(type,
                                              "warning" = "border-left:5px solid orange;background:#fff4e5;color:orange;",
                                              "error"   = "border-left:5px solid red;background:#fdecea;color:red;",
                                              "border-left:5px solid #3e6da9;background:#f0f4fb;color:#3e6da9;")
                              sprintf(
                                "<div style='padding:12px 14px;margin:8px;border-radius:10px;%s'>
           <div style='font-weight:700;margin-bottom:4px'>%s</div>
           <div>%s</div>
         </div>",
                                style, toupper(type), .html_escape(message))
                            },
                            
                            # ✅ JS 없는 아코디언 (details/summary)
                            generate_accordion = function(title = "Instructions", content = "") {
                              
                              css <- paste0(
                                "<style>",
                                "details.jacc{border:0;border-radius:8px;overflow:hidden;margin-top:20px}",
                                "details.jacc>summary{list-style:none;cursor:pointer;background:#3498db;color:#fff;",
                                "padding:8px 15px;display:flex;align-items:center;font-size:16px}",
                                "details.jacc>summary::-webkit-details-marker{display:none}",
                                "details.jacc>summary svg{margin-right:12px}",
                                "details.jacc[open] summary .v{transform:scaleY(0);transition:transform .2s ease}",
                                ".jacc .panel{background:#fff;padding:10px 15px}",
                                ".jacc a{color:#2563eb;text-decoration:underline}",
                                "</style>"
                              )
                              
                              header <- sprintf(
                                "<summary aria-expanded='false'>
           <svg width='20' height='18' viewBox='0 0 24 24' aria-hidden='true'>
             <circle cx='12' cy='12' r='11' fill='#fff'></circle>
             <rect x='5' y='11' width='15' height='3' fill='#3498db'></rect>
             <rect class='v' x='11' y='5' width='3' height='15' fill='#3498db'></rect>
           </svg>
           <span style='font-size:16px;'>%s</span>
         </summary>",
                                .html_escape(title)
                              )
                              
                              panel <- paste0("<div class='panel'>", content, "</div>")
                              paste0(css, "<details class='jacc'>", header, panel, "</details>")
                            }
                          )
)
# ---- end definition ----
