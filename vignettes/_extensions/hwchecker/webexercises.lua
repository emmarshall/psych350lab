-- webexercises.lua
function Pandoc(doc)
  quarto.doc.addHtmlDependency({
    name = "webexercises",
    version = "1.0.0",
    scripts = {"webex.js"},        -- make sure this matches your actual file
    stylesheets = {"webex.css"}
  })
  return doc
end
