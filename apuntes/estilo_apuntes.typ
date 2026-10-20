// Estilo de los apuntes (mismos colores que la presentación y el programa del curso)
#let azul = rgb("#1D3557")
#let rojo = rgb("#E63946")
#set par(justify: true)
#set page(footer: context [
  #set text(size: 7.5pt, fill: rgb("#6b7785"))
  Estadística básica con R para la investigación en ELE · Adrián Cabedo Nebot
  #h(1fr) #counter(page).display()
])
#show heading.where(level: 1): it => {
  pagebreak(weak: true)
  block(width: 100%, fill: azul, inset: (x: 14pt, y: 12pt), radius: 6pt, below: 12pt)[
    #set text(fill: white, size: 17pt, weight: "bold")
    #it
  ]
}
#show heading.where(level: 2): it => block(above: 1.6em, below: .8em, breakable: false)[
  #text(fill: azul, size: 12.5pt, weight: "bold")[#it]
  #v(-7pt)
  #line(length: 100%, stroke: 1pt + rojo)
]
#show raw: set text(font: "Menlo", size: 7.6pt)
#show raw.where(block: true): it => block(width: 100%, fill: rgb("#F4F6F9"), inset: 7pt, radius: 4pt,
  stroke: (left: 2.5pt + rgb("#A8DADC")))[#it]
#show table.cell.where(y: 0): set text(fill: white, weight: "bold")
#set table(fill: (x, y) => if y == 0 { azul } else if calc.even(y) { rgb("#F1F5F9") }, stroke: none, inset: 5pt)
#show table: set text(size: 8.5pt)
#show table: set par(justify: false)
#show link: set text(fill: rgb("#457B9D"))
