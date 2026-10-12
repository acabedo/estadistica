/* Diapositivas explicativas («En pocas palabras»).
   Cada diapositiva con un bloque `::: explicacion` en sus notas del orador recibe una diapositiva
   vertical debajo (flecha ↓) con ese mismo texto. Así el texto se escribe una sola vez en el .qmd
   y aparece en la vista del orador (tecla S) y en la diapositiva.
   Este script se carga al final de las diapositivas, ANTES de que se inicie reveal.js. */
(function () {
  const slides = document.querySelector(".reveal .slides");
  if (!slides) return;

  [...slides.children].forEach(sec => {
    if (sec.tagName !== "SECTION") return;
    const exp = sec.querySelector("aside.notes .explicacion");
    if (!exp) return;

    const h2 = sec.querySelector("h2");
    const nueva = document.createElement("section");
    nueva.className = "slide level2 explicacion-slide";
    if (sec.id) nueva.id = sec.id + "-explicacion";
    nueva.innerHTML = `<p class="exp-kicker">En pocas palabras</p><h2>${h2 ? h2.innerHTML : ""}</h2><div class="exp-cuerpo"></div>`;
    nueva.querySelector(".exp-cuerpo").append(...exp.cloneNode(true).childNodes);
    const notas = document.createElement("aside");
    notas.className = "notes";
    notas.appendChild(exp.cloneNode(true));
    nueva.appendChild(notas);

    // Pila vertical: la diapositiva original arriba y la explicación debajo.
    // data-start-indexv="0" hace que al llegar con → se entre siempre por la original.
    const pila = document.createElement("section");
    pila.setAttribute("data-start-indexv", "0");
    sec.replaceWith(pila);
    pila.append(sec, nueva);

    const boton = document.createElement("button");
    boton.className = "ir-explicacion";
    boton.type = "button";
    boton.title = "Explicación de esta diapositiva (flecha ↓)";
    boton.textContent = "↓ En pocas palabras";
    boton.addEventListener("click", () => window.Reveal && window.Reveal.down());
    sec.appendChild(boton);
  });

  // Cuando reveal.js esté listo: numerar solo las diapositivas horizontales y hacer que
  // la barra espaciadora y las teclas de los punteros (AvPág/RePág) no entren en las explicaciones.
  document.addEventListener("DOMContentLoaded", () => {
    const R = window.Reveal;
    if (!R) return;
    const ajustar = () => R.configure({
      slideNumber: s => {
        const h = R.getIndices(s).h + 1, total = R.getHorizontalSlides().length;
        return [s && s.classList.contains("explicacion-slide") ? `${h} ↓` : `${h}`, "/", total];   // total debe ser un número
      },
      keyboard: { 32: "right", 34: "right", 33: "left" }
    });
    if (R.isReady()) ajustar(); else R.on("ready", ajustar);
  });
})();
