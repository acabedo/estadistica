/* Componentes interactivos del curso «Estadística básica con R para la investigación en ELE».
   Cada diapositiva interactiva contiene <div class="w" data-w="nombre">; aquí se registra W.nombre.
   Los datos y los resultados de R llegan en window.CURSO (interactivo/datos_curso.js). */
(function () {
  "use strict";

  const COL = { azul: "#1D3557", rojo: "#E63946", verde: "#2A9D8F", arena: "#F4A261",
    celeste: "#457B9D", agua: "#A8DADC", gris: "#9aa5b1", claro: "#e6ebf1", tinta: "#22313f" };
  const COL_L1 = { "árabe": COL.azul, "chino": COL.rojo, "inglés": COL.verde, "italiano": COL.arena };
  const NIVELES = ["A2", "B1", "B2", "C1"];
  const L1S = ["árabe", "chino", "inglés", "italiano"];
  const CUR = () => window.CURSO;
  const W = {};

  /* ---------- formato ---------- */
  const f = (x, d = 2) => (isFinite(x) ? (+x).toFixed(d).replace(".", ",") : "—");
  const fp = p => (p < 0.001 ? "< 0,001" : "= " + f(p, 3));
  const pct = (x, d = 0) => f(100 * x, d) + " %";
  const signo = (x, d = 1) => (x > 0 ? "+" : "") + f(x, d);

  /* ---------- distribuciones ---------- */
  function lgamma(x) {
    const c = [76.18009172947146, -86.50532032941677, 24.01409824083091, -1.231739572450155,
      0.1208650973866179e-2, -0.5395239384953e-5];
    let y = x, t = x + 5.5; t -= (x + 0.5) * Math.log(t);
    let s = 1.000000000190015; for (let j = 0; j < 6; j++) s += c[j] / ++y;
    return -t + Math.log(2.5066282746310005 * s / x);
  }
  function betacf(a, b, x) {
    const FP = 1e-300; let qab = a + b, qap = a + 1, qam = a - 1, c = 1, d = 1 - qab * x / qap;
    if (Math.abs(d) < FP) d = FP; d = 1 / d; let h = d;
    for (let m = 1; m <= 300; m++) {
      const m2 = 2 * m; let aa = m * (b - m) * x / ((qam + m2) * (a + m2));
      d = 1 + aa * d; if (Math.abs(d) < FP) d = FP; c = 1 + aa / c; if (Math.abs(c) < FP) c = FP;
      d = 1 / d; h *= d * c;
      aa = -(a + m) * (qab + m) * x / ((a + m2) * (qap + m2));
      d = 1 + aa * d; if (Math.abs(d) < FP) d = FP; c = 1 + aa / c; if (Math.abs(c) < FP) c = FP;
      d = 1 / d; const del = d * c; h *= del; if (Math.abs(del - 1) < 3e-14) break;
    }
    return h;
  }
  function ibeta(x, a, b) {
    if (x <= 0) return 0; if (x >= 1) return 1;
    const bt = Math.exp(lgamma(a + b) - lgamma(a) - lgamma(b) + a * Math.log(x) + b * Math.log(1 - x));
    return x < (a + 1) / (a + b + 2) ? bt * betacf(a, b, x) / a : 1 - bt * betacf(b, a, 1 - x) / b;
  }
  const pt2 = (t, df) => ibeta(df / (df + t * t), df / 2, 0.5);          // p bilateral
  const pf = (F, d1, d2) => (F <= 0 ? 1 : ibeta(d2 / (d2 + d1 * F), d2 / 2, d1 / 2));
  const dt = (t, df) => Math.exp(lgamma((df + 1) / 2) - lgamma(df / 2)) / Math.sqrt(df * Math.PI) *
    Math.pow(1 + t * t / df, -(df + 1) / 2);
  const dnorm = (x, m = 0, s = 1) => Math.exp(-0.5 * ((x - m) / s) ** 2) / (s * Math.sqrt(2 * Math.PI));
  function erf(x) {
    const s = Math.sign(x); x = Math.abs(x); const t = 1 / (1 + 0.3275911 * x);
    const y = 1 - (((((1.061405429 * t - 1.453152027) * t) + 1.421413741) * t - 0.284496736) * t + 0.254829592) * t * Math.exp(-x * x);
    return s * y;
  }
  const pnorm = z => 0.5 * (1 + erf(z / Math.SQRT2));
  function qnorm(p) {
    const a = [-39.69683028665376, 220.9460984245205, -275.9285104469687, 138.357751867269, -30.66479806614716, 2.506628277459239];
    const b = [-54.47609879822406, 161.5858368580409, -155.6989798598866, 66.80131188771972, -13.28068155288572];
    const c = [-0.007784894002430293, -0.3223964580411365, -2.400758277161838, -2.549732539343734, 4.374664141464968, 2.938163982698783];
    const d = [0.007784695709041462, 0.3224671290700398, 2.445134137142996, 3.754408661907416];
    const pl = 0.02425; let q, r;
    if (p < pl) { q = Math.sqrt(-2 * Math.log(p)); return (((((c[0] * q + c[1]) * q + c[2]) * q + c[3]) * q + c[4]) * q + c[5]) / ((((d[0] * q + d[1]) * q + d[2]) * q + d[3]) * q + 1); }
    if (p > 1 - pl) { q = Math.sqrt(-2 * Math.log(1 - p)); return -(((((c[0] * q + c[1]) * q + c[2]) * q + c[3]) * q + c[4]) * q + c[5]) / ((((d[0] * q + d[1]) * q + d[2]) * q + d[3]) * q + 1); }
    q = p - 0.5; r = q * q;
    return (((((a[0] * r + a[1]) * r + a[2]) * r + a[3]) * r + a[4]) * r + a[5]) * q / (((((b[0] * r + b[1]) * r + b[2]) * r + b[3]) * r + b[4]) * r + 1);
  }
  function qt(p, df) {            // cuantil superior de t: P(T > q) = 1 - p
    let lo = 0, hi = 50;
    for (let i = 0; i < 80; i++) { const m = (lo + hi) / 2; (1 - pt2(m, df) / 2 < p) ? lo = m : hi = m; }
    return (lo + hi) / 2;
  }
  function rng(seed) {
    let a = seed >>> 0;
    return function () { a |= 0; a = a + 0x6D2B79F5 | 0; let t = Math.imul(a ^ a >>> 15, 1 | a);
      t = t + Math.imul(t ^ t >>> 7, 61 | t) ^ t; return ((t ^ t >>> 14) >>> 0) / 4294967296; };
  }
  function rnormGen(r) { return () => { let u = 0, v = 0; while (u === 0) u = r(); v = r(); return Math.sqrt(-2 * Math.log(u)) * Math.cos(2 * Math.PI * v); }; }
  const media = a => d3.mean(a), de = a => d3.deviation(a);
  function welch(a, b) {
    const ma = media(a), mb = media(b), va = d3.variance(a), vb = d3.variance(b), na = a.length, nb = b.length;
    const se2 = va / na + vb / nb, t = (ma - mb) / Math.sqrt(se2);
    const gl = se2 * se2 / ((va / na) ** 2 / (na - 1) + (vb / nb) ** 2 / (nb - 1));
    const sp = Math.sqrt(((na - 1) * va + (nb - 1) * vb) / (na + nb - 2));
    return { t, gl, p: pt2(t, gl), d: (ma - mb) / sp, ma, mb };
  }
  function pearson(x, y) {
    const mx = media(x), my = media(y); let sxy = 0, sxx = 0, syy = 0;
    x.forEach((v, i) => { sxy += (v - mx) * (y[i] - my); sxx += (v - mx) ** 2; syy += (y[i] - my) ** 2; });
    return sxy / Math.sqrt(sxx * syy);
  }
  function rangos(v) {
    const o = v.map((x, i) => [x, i]).sort((a, b) => a[0] - b[0]); const r = new Array(v.length);
    for (let i = 0; i < o.length;) { let j = i; while (j + 1 < o.length && o[j + 1][0] === o[i][0]) j++;
      const m = (i + j) / 2 + 1; for (let k = i; k <= j; k++) r[o[k][1]] = m; i = j + 1; }
    return r;
  }
  const gini = (a, b) => { const n = a + b; if (!n) return 0; const p = a / n; return 1 - p * p - (1 - p) * (1 - p); };

  /* ---------- utilidades de interfaz ---------- */
  function crear(padre, cls, tag = "div") { const n = document.createElement(tag); if (cls) n.className = cls; padre.appendChild(n); return n; }
  function marco(el, op = {}) {
    el.classList.add("w-on"); if (op.abajo) el.classList.add("w-abajo"); if (op.izq) el.classList.add("w-izq");
    if (op.ancho) el.style.setProperty("--panel", op.ancho + "px");
    const vis = crear(el, "w-vis"), panel = crear(el, "w-panel"); return { vis, panel };
  }
  const lienzo = (padre, w, h) => d3.select(padre).append("svg").attr("viewBox", `0 0 ${w} ${h}`).attr("class", "w-svg");
  function deslizador(panel, o) {
    const box = crear(panel, "w-ctrl"), lab = crear(box, "w-lab"), inp = crear(box, "", "input");
    Object.assign(inp, { type: "range", min: o.min, max: o.max, step: o.paso || 1, value: o.valor });
    const fm = o.fmt || (v => v); const upd = () => { lab.innerHTML = `${o.etq} <b>${fm(+inp.value)}</b>`; }; upd();
    inp.addEventListener("input", () => { upd(); o.cambio(+inp.value); });
    return { get v() { return +inp.value; }, set(v, silencio) { inp.value = v; upd(); if (!silencio) o.cambio(+inp.value); }, inp };
  }
  function botones(panel, lista) {
    const box = crear(panel, "w-btns");
    return lista.map(b => { const x = crear(box, "w-btn" + (b.cls ? " " + b.cls : ""), "button"); x.innerHTML = b.etq; x.onclick = b.click; return x; });
  }
  function segmentado(panel, etq, opciones, valor, cambio) {
    const box = crear(panel, "w-ctrl"); if (etq) crear(box, "w-lab").innerHTML = etq;
    const fila = crear(box, "w-seg"); let actual = valor;
    const bs = opciones.map(o => { const [v, t] = Array.isArray(o) ? o : [o, o];
      const b = crear(fila, "", "button"); b.innerHTML = t; b.dataset.v = v; b.onclick = () => api.set(v); return b; });
    const api = { get v() { return actual; }, set(v, silencio) { actual = v; bs.forEach(b => b.classList.toggle("on", b.dataset.v == v)); if (!silencio) cambio(v); } };
    api.set(valor, true); return api;
  }
  function selector(panel, etq, opciones, valor, cambio) {
    const box = crear(panel, "w-ctrl"); crear(box, "w-lab").innerHTML = etq; const s = crear(box, "w-sel", "select");
    opciones.forEach(o => { const [v, t] = Array.isArray(o) ? o : [o, o]; const op = crear(s, "", "option"); op.value = v; op.textContent = t; });
    s.value = valor; s.onchange = () => cambio(s.value); return s;
  }
  const lectura = (panel, cls = "") => crear(panel, "w-read " + cls);
  function ejes(g, x, y, o = {}) {
    g.selectAll(".eje").remove();
    const ax = g.append("g").attr("class", "eje").attr("transform", `translate(0,${o.y0 ?? y.range()[0]})`)
      .call(d3.axisBottom(x).ticks(o.tx || 6).tickFormat(o.fx || (v => String(v).replace(".", ","))));
    if (o.xlab) ax.append("text").attr("class", "eje-t").attr("x", (x.range()[0] + x.range()[1]) / 2).attr("y", 42).text(o.xlab);
    if (y && !o.sinY) {
      const ay = g.append("g").attr("class", "eje").attr("transform", `translate(${o.x0 ?? x.range()[0]},0)`)
        .call(d3.axisLeft(y).ticks(o.ty || 5).tickFormat(o.fy || (v => String(v).replace(".", ","))));
      if (o.ylab) ay.append("text").attr("class", "eje-t").attr("transform", "rotate(-90)")
        .attr("x", -(y.range()[0] + y.range()[1]) / 2).attr("y", -46).text(o.ylab);
    }
  }
  function kde(valores, dominio, bw, k = 80) {
    const xs = d3.range(k).map(i => dominio[0] + i * (dominio[1] - dominio[0]) / (k - 1));
    return xs.map(x => [x, d3.mean(valores, v => dnorm(x, v, bw))]);
  }
  const silverman = v => 1.06 * de(v) * Math.pow(v.length, -0.2);
  const T = (ms = 700) => d3.transition().duration(ms).ease(d3.easeCubicInOut);

  /* =====================================================================
     SESIÓN 1 · DESCRIPTIVA
     ===================================================================== */

  W.muestra = el => {
    const D = CUR().ele, { vis, panel } = marco(el), w = 900, h = 500, svg = lienzo(vis, w, h), r = rng(7);
    const pts = D.map((d, i) => ({ i, nota: d.nota_dele, x: 40 + r() * 820, y: 24 + r() * 290 }));
    const mu = media(D.map(d => d.nota_dele));
    const col = d3.scaleSequential([35, 100], d3.interpolateRgb("#d8e2ec", COL.azul));
    const dots = svg.append("g").selectAll("circle").data(pts).join("circle")
      .attr("cx", d => d.x).attr("cy", d => d.y).attr("r", 6).attr("fill", "#cdd6e0");
    svg.append("text").attr("class", "w-tag").attr("x", 40).attr("y", 16).text("Población: 240 estudiantes (cada punto, una persona; el color, su nota DELE)");
    const x = d3.scaleLinear([56, 80], [60, 860]), g = svg.append("g");
    ejes(g, x, null, { y0: 470, sinY: true, tx: 12 });
    svg.append("line").attr("x1", x(mu)).attr("x2", x(mu)).attr("y1", 340).attr("y2", 470).attr("stroke", COL.rojo).attr("stroke-width", 3);
    svg.append("text").attr("class", "w-tag rojo").attr("x", x(mu) + 6).attr("y", 352).text("nota media real " + f(mu, 1));
    svg.append("text").attr("class", "w-tag").attr("x", 60).attr("y", 352).text("Nota media de cada muestra extraída");
    const gm = svg.append("g"); let hist = [];
    let n = 30; const rd = lectura(panel, "grande");
    const nDesl = deslizador(panel, { etq: "Tamaño de la muestra (n)", min: 5, max: 120, valor: 30, cambio: v => { n = v; } });
    function extraer(rapido) {
      const idx = d3.shuffle(d3.range(D.length)).slice(0, n), s = new Set(idx);
      const m = media(idx.map(i => D[i].nota_dele)); hist.push(m);
      dots.interrupt().transition().duration(rapido ? 150 : 500).attr("r", d => s.has(d.i) ? 9 : 5)
        .attr("fill", d => s.has(d.i) ? col(d.nota) : "#dfe5ec").attr("stroke", d => s.has(d.i) ? "#fff" : "none");
      const cuenta = {};
      gm.selectAll("circle").data(hist).join(en => en.append("circle").attr("cy", 340).attr("opacity", 0))
        .attr("cx", v => x(Math.max(56, Math.min(80, v))))
        .attr("r", 4.5).attr("fill", (v, i) => i === hist.length - 1 ? COL.rojo : COL.celeste)
        .transition().duration(rapido ? 100 : 600).attr("opacity", 0.85)
        .attr("cy", v => { const k = Math.round(v * 2) / 2; cuenta[k] = (cuenta[k] || 0) + 1; return 462 - (cuenta[k] - 1) * 8; });
      rd.innerHTML = `<div>Nota media real <b class="rojo">${f(mu, 1)}</b></div><div>Nota media de esta muestra <b>${f(m, 1)}</b></div>
        <div>Error de muestreo <b>${signo(m - mu)}</b></div><div class="peq">${hist.length} muestras · DE de las medias ${hist.length > 2 ? f(de(hist), 2) : "—"}</div>`;
    }
    botones(panel, [{ etq: "Extraer una muestra", click: () => extraer() },
      { etq: "+ 25 muestras", cls: "sec", click: () => { let k = 0; const t = d3.interval(() => { extraer(true); if (++k >= 25) t.stop(); }, 90); } },
      { etq: "Reiniciar", cls: "ter", click: () => { hist = []; gm.selectAll("circle").remove(); dots.attr("r", 6).attr("fill", "#cdd6e0").attr("stroke", "none"); rd.innerHTML = ""; } }]);
    crear(panel, "w-nota").innerHTML = "Prueba con <b>n = 5</b> y luego con <b>n = 120</b>: ¿qué les pasa a las medias?";
    nDesl;
  };

  W["media-mediana"] = el => {
    const { vis, panel } = marco(el), w = 900, h = 360, svg = lienzo(vis, w, h);
    const base = [12, 14, 13, 15, 14]; const x = d3.scaleLinear([0, 125], [40, 870]);
    const g = svg.append("g"); ejes(g, x, null, { y0: 250, sinY: true, xlab: "Tiempo (segundos)" });
    const pts = svg.append("g"), marcas = svg.append("g"); const rd = lectura(panel, "grande");
    function dibuja(v) {
      const datos = base.concat([v]), m = media(datos), md = d3.median(datos), cnt = {};
      pts.selectAll("circle").data(datos).join("circle").attr("r", 13)
        .attr("fill", (d, i) => i === 5 ? COL.arena : COL.azul).attr("stroke", "#fff").attr("stroke-width", 2)
        .attr("cy", d => { cnt[d] = (cnt[d] || 0) + 1; return 232 - (cnt[d] - 1) * 26; })
        .transition().duration(250).attr("cx", d => x(d));
      const M = [{ k: "media", v: m, c: COL.rojo, y: 70 }, { k: "mediana", v: md, c: COL.verde, y: 120 }];
      const s = marcas.selectAll("g").data(M).join(en => { const gg = en.append("g"); gg.append("line"); gg.append("path"); gg.append("text"); return gg; });
      s.transition().duration(300).attr("transform", d => `translate(${x(d.v)},0)`);
      s.select("line").attr("y1", d => d.y + 6).attr("y2", 250).attr("stroke", d => d.c).attr("stroke-width", 3).attr("stroke-dasharray", "6 4");
      s.select("path").attr("d", d => `M-10,${d.y - 12}L10,${d.y - 12}L0,${d.y + 4}Z`).attr("fill", d => d.c);
      s.select("text").attr("y", d => d.y - 18).attr("text-anchor", "middle").attr("class", "w-tag fuerte")
        .attr("fill", d => d.c).text(d => `${d.k} ${f(d.v, 1)}`);
      rd.innerHTML = `<div>Media <b class="rojo">${f(m, 1)}</b></div><div>Mediana <b class="verde">${f(md, 1)}</b></div>
        <div class="peq">La media se desplaza ${f(m - 13.6, 1)} s respecto a los cinco valores típicos; la mediana apenas se mueve.</div>`;
    }
    deslizador(panel, { etq: "Valor del sexto estudiante", min: 10, max: 120, valor: 95, cambio: dibuja });
    crear(panel, "w-nota").innerHTML = "Arrastra el punto <b style='color:#F4A261'>naranja</b> hacia la derecha: solo la <b>media</b> lo persigue.";
    dibuja(95);
  };

  W.dispersion = el => {
    const { vis, panel } = marco(el), w = 900, h = 440, svg = lienzo(vis, w, h);
    const x = d3.scaleLinear([20, 120], [50, 870]), y = d3.scaleLinear([0, 0.1], [330, 20]);
    ejes(svg.append("g"), x, null, { y0: 330, sinY: true, xlab: "Nota" });
    const z = d3.range(60).map(rnormGen(rng(3)));
    const grupos = [{ k: "A", c: COL.verde, sd: 4, yb: 365 }, { k: "B", c: COL.rojo, sd: 15, yb: 405 }];
    const area = d3.area().x(d => x(d[0])).y0(330).y1(d => y(d[1])).curve(d3.curveBasis);
    const paths = svg.append("g").selectAll("path").data(grupos).join("path").attr("fill-opacity", 0.45).attr("stroke-width", 2.5)
      .attr("fill", d => d.c).attr("stroke", d => d.c);
    const tiras = svg.append("g").selectAll("g").data(grupos).join("g");
    svg.append("line").attr("x1", x(70)).attr("x2", x(70)).attr("y1", 20).attr("y2", 410).attr("stroke", COL.tinta).attr("stroke-dasharray", "5 4");
    const rd = lectura(panel, "grande");
    function dibuja() {
      paths.transition().duration(400).attr("d", g => area(d3.range(20, 120.5, 0.5).map(v => [v, Math.min(0.1, dnorm(v, 70, g.sd))])));
      tiras.each(function (g) {
        d3.select(this).selectAll("circle").data(z).join("circle").attr("r", 4).attr("fill", g.c).attr("opacity", 0.6)
          .attr("cy", (d, i) => g.yb + ((i % 5) - 2) * 3).transition().duration(400).attr("cx", d => x(Math.max(20, Math.min(120, 70 + d * g.sd))));
      });
      rd.innerHTML = grupos.map(g => `<div><b style="color:${g.c}">Grupo ${g.k}</b> DE ${f(g.sd, 1)} · RIC ≈ ${f(1.349 * g.sd, 1)}</div><div class="peq">2 de cada 3 notas entre ${f(70 - g.sd, 0)} y ${f(70 + g.sd, 0)} · mitad central entre ${f(70 - 0.674 * g.sd, 1)} y ${f(70 + 0.674 * g.sd, 1)} · varianza ${f(g.sd * g.sd, 0)}</div>`).join("") +
        `<div class="peq">Las dos tienen media 70. Cambia solo la dispersión. <b>DE</b> = desviación típica (o estándar); <b>RIC</b> = Q3 − Q1.</div>`;
    }
    grupos.forEach(g => deslizador(panel, { etq: `DE del grupo ${g.k}`, min: 2, max: 20, paso: 0.5, valor: g.sd, fmt: v => f(v, 1), cambio: v => { g.sd = v; dibuja(); } }));
    dibuja();
  };

  W["de-pasos"] = el => {
    const { vis, panel } = marco(el), w = 900, h = 480, svg = lienzo(vis, w, h);
    const v = [55, 62, 70, 71, 92], m = 70, x = d3.scaleLinear([0.5, 5.5], [70, 560]), y = d3.scaleLinear([40, 100], [440, 30]);
    const u = y(0) - y(1);                       // píxeles por punto de nota
    ejes(svg.append("g"), x, y, { tx: 5, xlab: "Estudiante", ylab: "Nota", fx: d3.format("d") });
    const gl = svg.append("g"), gs = svg.append("g"), gseg = svg.append("g"), gp = svg.append("g"), gx = svg.append("g");
    gl.append("line").attr("x1", 70).attr("x2", 560).attr("y1", y(m)).attr("y2", y(m)).attr("stroke", COL.rojo).attr("stroke-width", 3);
    gl.append("text").attr("x", 565).attr("y", y(m) + 5).attr("class", "w-tag rojo fuerte").text("media = 70");
    gp.selectAll("circle").data(v).join("circle").attr("cx", (d, i) => x(i + 1)).attr("cy", d => y(d)).attr("r", 10).attr("fill", COL.azul);
    const textos = [
      "<b>1.</b> Cinco notas y su media (70).",
      "<b>2.</b> Distancia de cada nota a la media: −15, −8, 0, +1, +22. Si las sumamos dan 0.",
      "<b>3.</b> Elevamos cada distancia al cuadrado (literalmente: cuadrados): 225, 64, 0, 1 y 484. Desaparecen los signos.",
      "<b>4.</b> Sumamos los cuadrados: 225 + 64 + 0 + 1 + 484 = <b>774</b> (suma de cuadrados). Los promediamos dividiendo entre n − 1 = 4: <b>varianza = 774 / 4 = 193,5</b> (en puntos²).",
      "<b>5.</b> Raíz cuadrada para volver a puntos de nota: <b>DE = √193,5 ≈ 13,9</b>. La franja azul va de 70 − 13,9 = 56,1 a 70 + 13,9 = 83,9 (media ± 1 DE): dentro quedan 62, 70 y 71."];
    const rd = lectura(panel, "grande"); let paso = 0;
    function dibuja() {
      const segs = paso >= 1 ? v : [];
      gseg.selectAll("line").data(segs).join(en => en.append("line").attr("y2", y(m)))
        .attr("x1", (d, i) => x(i + 1)).attr("x2", (d, i) => x(i + 1)).attr("y1", y(m)).attr("stroke", COL.celeste)
        .attr("stroke-width", 3).attr("stroke-dasharray", "6 4").transition().duration(500).attr("y2", d => y(d));
      gseg.selectAll("text").data(paso >= 1 && paso < 3 ? v : []).join("text").attr("class", "w-tag fuerte")
        .attr("x", (d, i) => x(i + 1) - 14).attr("text-anchor", "end").attr("y", d => (y(d) + y(m)) / 2 + 5).text(d => signo(d - m, 0));
      const cuad = paso >= 2 && paso < 4 ? v : [];
      gs.selectAll("rect").data(cuad).join(en => en.append("rect").attr("width", 0).attr("height", 0))
        .attr("x", (d, i) => x(i + 1)).attr("y", d => Math.min(y(d), y(m)))
        .attr("fill", COL.arena).attr("fill-opacity", 0.35).attr("stroke", COL.arena).attr("stroke-width", 2)
        .transition().duration(600).attr("width", d => Math.abs(d - m) * u).attr("height", d => Math.abs(d - m) * u);
      gs.selectAll("text").data(cuad).join("text").attr("class", "w-tag fuerte").attr("x", (d, i) => x(i + 1) + 6)
        .attr("y", d => Math.min(y(d), y(m)) - 6).text(d => (d - m) ** 2);
      const sd = Math.sqrt(193.5);
      gx.selectAll("*").remove();
      if (paso === 3) {
        const lado = Math.sqrt(193.5) * u;
        gx.append("rect").attr("x", 640).attr("y", 120).attr("width", lado).attr("height", lado).attr("fill", COL.arena).attr("fill-opacity", .5).attr("stroke", COL.arena).attr("stroke-width", 3);
        gx.append("text").attr("x", 640).attr("y", 110).attr("class", "w-tag fuerte").text("cuadrado medio = 193,5");
      }
      if (paso >= 4) {
        gx.append("rect").attr("x", 70).attr("width", 490).attr("y", y(m + sd)).attr("height", y(m - sd) - y(m + sd)).attr("fill", COL.agua).attr("opacity", 0.35).lower();
        gx.lower();
        const lado = sd * u;
        gx.append("rect").attr("x", 640).attr("y", 120).attr("width", lado).attr("height", lado).attr("fill", COL.arena).attr("fill-opacity", .3).attr("stroke", COL.arena).attr("stroke-width", 2);
        gx.append("line").attr("x1", 640).attr("x2", 640 + lado).attr("y1", 120 + lado + 14).attr("y2", 120 + lado + 14).attr("stroke", COL.rojo).attr("stroke-width", 4);
        gx.append("text").attr("x", 640).attr("y", 120 + lado + 40).attr("class", "w-tag rojo fuerte").text("lado = DE = 13,9");
      }
      rd.innerHTML = textos[paso];
    }
    botones(panel, [{ etq: "← Anterior", cls: "sec", click: () => { paso = Math.max(0, paso - 1); dibuja(); } },
      { etq: "Siguiente paso →", click: () => { paso = Math.min(4, paso + 1); dibuja(); } }]);
    dibuja();
    return { enter() { paso = 0; dibuja(); } };
  };

  W.cuartiles = el => {
    const D = CUR().ele.map(d => d.nota_dele).sort(d3.ascending), { vis, panel } = marco(el), w = 900, h = 470, svg = lienzo(vis, w, h);
    const x = d3.scaleLinear([d3.min(D) - 2, d3.max(D) + 2], [40, 870]); const cnt = {};
    const pos = D.map(v => { cnt[v] = (cnt[v] || 0) + 1; return { v, k: cnt[v] }; });
    ejes(svg.append("g"), x, null, { y0: 300, sinY: true, xlab: "Nota DELE (cada punto, un estudiante)" });
    const sombra = svg.append("rect").attr("x", 40).attr("y", 60).attr("height", 240).attr("fill", COL.agua).attr("opacity", 0.35);
    const pts = svg.append("g").selectAll("circle").data(pos).join("circle").attr("cx", d => x(d.v)).attr("cy", d => 292 - (d.k - 1) * 11).attr("r", 5);
    const q = [0.25, 0.5, 0.75].map(p => d3.quantile(D, p)), ric = q[2] - q[0];
    const lo = d3.min(D.filter(v => v >= q[0] - 1.5 * ric)), hi = d3.max(D.filter(v => v <= q[2] + 1.5 * ric));
    const caja = svg.append("g").attr("transform", "translate(0,385)");
    caja.append("line").attr("x1", x(lo)).attr("x2", x(hi)).attr("stroke", COL.tinta).attr("stroke-width", 2);
    caja.append("rect").attr("x", x(q[0])).attr("width", x(q[2]) - x(q[0])).attr("y", -22).attr("height", 44).attr("fill", COL.agua).attr("stroke", COL.azul).attr("stroke-width", 2);
    caja.append("line").attr("x1", x(q[1])).attr("x2", x(q[1])).attr("y1", -22).attr("y2", 22).attr("stroke", COL.rojo).attr("stroke-width", 4);
    caja.append("text").attr("x", (x(q[0]) + x(q[2])) / 2).attr("y", 46).attr("text-anchor", "middle").attr("class", "w-tag fuerte").text(`RIC = Q3 − Q1 = ${f(ric, 1)}`);
    const linea = svg.append("line").attr("y1", 40).attr("y2", 300).attr("stroke", COL.rojo).attr("stroke-width", 3);
    const etq = svg.append("text").attr("y", 32).attr("class", "w-tag rojo fuerte").attr("text-anchor", "middle");
    const rd = lectura(panel, "grande");
    function dibuja(p) {
      const v = d3.quantile(D, p / 100);
      linea.transition().duration(300).attr("x1", x(v)).attr("x2", x(v)); etq.transition().duration(300).attr("x", x(v)).text(`P${p} = ${f(v, 1)}`);
      sombra.transition().duration(300).attr("width", x(v) - 40);
      pts.attr("fill", d => d.v <= v ? COL.azul : "#c9d3df");
      const nombre = { 25: "Q1", 50: "la mediana", 75: "Q3" }[p];
      rd.innerHTML = `<div>El <b>${p} %</b> de los estudiantes tiene una nota ≤ <b class="rojo">${f(v, 1)}</b></div>` + (nombre ? `<div>Es <b>${nombre}</b>.</div>` : "");
    }
    const s = deslizador(panel, { etq: "Percentil", min: 1, max: 99, valor: 50, cambio: dibuja });
    botones(panel, [{ etq: "Q1", cls: "sec", click: () => s.set(25) }, { etq: "Mediana", cls: "sec", click: () => s.set(50) }, { etq: "Q3", cls: "sec", click: () => s.set(75) }]);
    dibuja(50);
  };

  W["resumen-grupos"] = el => {
    const D = CUR().ele, { vis, panel } = marco(el, { ancho: 420 }), w = 860, h = 470, svg = lienzo(vis, w, h);
    const st = { g: "L1", v: "nota_dele", e: "media" };
    const orden = { L1: L1S, nivel: NIVELES, estancia: ["no", "sí"], subjuntivo: ["correcto", "incorrecto"] };
    const gEj = svg.append("g"), gP = svg.append("g"), gM = svg.append("g"); const cod = crear(panel, "w-code", "pre"), tabla = crear(panel, "w-tabla");
    function dibuja() {
      const niv = orden[st.g], x = d3.scalePoint(niv, [90, 820]).padding(0.5);
      const ext = d3.extent(D, d => d[st.v]), y = d3.scaleLinear([ext[0], ext[1]], [420, 30]).nice();
      ejes(gEj, x, y, { ylab: st.v, xlab: st.g });
      const r = rng(11);
      gP.selectAll("circle").data(D, d => d.id).join("circle").attr("r", 4).attr("opacity", 0.35)
        .transition().duration(700).attr("cx", d => x(d[st.g]) + (r() - 0.5) * 70).attr("cy", d => y(d[st.v]))
        .attr("fill", d => st.g === "L1" ? COL_L1[d.L1] : COL.celeste);
      const res = niv.map(k => { const a = D.filter(d => d[st.g] === k).map(d => d[st.v]);
        return { k, n: a.length, c: st.e === "media" ? media(a) : d3.median(a), s: de(a) }; });
      const s = gM.selectAll("g").data(res, d => d.k).join(en => { const g = en.append("g"); g.append("line").attr("class", "bigote"); g.append("rect"); g.append("text"); return g; });
      s.transition().duration(700).attr("transform", d => `translate(${x(d.k)},0)`);
      s.select(".bigote").transition().duration(700).attr("y1", d => y(d.c - d.s)).attr("y2", d => y(d.c + d.s)).attr("stroke", COL.tinta).attr("stroke-width", 3);
      s.select("rect").transition().duration(700).attr("x", -40).attr("width", 80).attr("y", d => y(d.c) - 3).attr("height", 6).attr("fill", COL.rojo);
      s.select("text").attr("x", 46).attr("class", "w-tag fuerte").transition().duration(700).attr("y", d => y(d.c) + 5).text(d => f(d.c, 1));
      const fn = st.e === "media" ? "mean" : "median";
      cod.innerHTML = `ele |&gt;\n  group_by(<b>${st.g}</b>) |&gt;\n  summarise(n = n(),\n            ${st.e} = ${fn}(<b>${st.v}</b>),\n            de = sd(<b>${st.v}</b>))`;
      tabla.innerHTML = `<table><tr><th>${st.g}</th><th>n</th><th>${st.e}</th><th>de</th></tr>` +
        res.map(d => `<tr><td>${d.k}</td><td>${d.n}</td><td>${f(d.c, 1)}</td><td>${f(d.s, 1)}</td></tr>`).join("") + "</table>";
    }
    selector(panel, "Agrupar por", ["L1", "nivel", "estancia", "subjuntivo"], st.g, v => { st.g = v; dibuja(); });
    selector(panel, "Variable", ["nota_dele", "horas", "err_ser_estar"], st.v, v => { st.v = v; dibuja(); });
    segmentado(panel, "Centro", ["media", "mediana"], "media", v => { st.e = v; dibuja(); });
    panel.appendChild(cod); panel.appendChild(tabla); dibuja();
  };

  /* =====================================================================
     SESIÓN 2 · VISUALIZACIÓN
     ===================================================================== */

  W.anscombe = el => {
    const A = CUR().anscombe, { vis, panel } = marco(el), w = 860, h = 470, svg = lienzo(vis, w, h);
    const x = d3.scaleLinear([3, 20], [70, 830]), y = d3.scaleLinear([2, 14], [420, 20]);
    ejes(svg.append("g"), x, y, { xlab: "x", ylab: "y" });
    svg.append("line").attr("x1", x(3)).attr("x2", x(20)).attr("y1", y(3 + 0.5 * 3)).attr("y2", y(3 + 0.5 * 20)).attr("stroke", COL.rojo).attr("stroke-width", 3);
    const pts = svg.append("g"); const rd = lectura(panel, "grande"); let auto = null;
    function dibuja(s) {
      const d = A.filter(a => a.set === String(s));
      pts.selectAll("circle").data(d, (a, i) => i).join("circle").attr("r", 10).attr("fill", COL.azul).attr("stroke", "#fff").attr("stroke-width", 2)
        .transition().duration(1100).ease(d3.easeCubicInOut).attr("cx", a => x(a.x)).attr("cy", a => y(a.y));
      const xs = d.map(a => a.x), ys = d.map(a => a.y);
      rd.innerHTML = `<div class="titulo-read">Conjunto ${["I", "II", "III", "IV"][s - 1]}</div>
        <div>media de x <b>${f(media(xs), 2)}</b></div><div>media de y <b>${f(media(ys), 2)}</b></div>
        <div>varianza de y <b>${f(d3.variance(ys), 2)}</b></div><div>correlación r <b>${f(pearson(xs, ys), 3)}</b></div>
        <div>recta <b>y = 3 + 0,5x</b></div>`;
      seg.set(String(s), true);
    }
    const seg = segmentado(panel, "Conjunto", [["1", "I"], ["2", "II"], ["3", "III"], ["4", "IV"]], "1", v => { if (auto) { auto.stop(); auto = null; } dibuja(+v); });
    panel.appendChild(rd);
    botones(panel, [{ etq: "▶ Animar", cls: "sec", click: () => { if (auto) { auto.stop(); auto = null; return; } let s = +seg.v; auto = d3.interval(() => { s = s % 4 + 1; dibuja(s); }, 2200); } }]);
    dibuja(1);
  };

  W.capas = el => {
    const D = CUR().ele, { vis, panel } = marco(el, { izq: true, ancho: 520 }), w = 860, h = 480, svg = lienzo(vis, w, h);
    const pasos = [
      ["ggplot(ele)"],
      ["ggplot(ele, aes(x = horas, y = nota_dele))"],
      ["ggplot(ele, aes(x = horas, y = nota_dele)) +", "  geom_point()"],
      ["ggplot(ele, aes(x = horas, y = nota_dele,", "                colour = estancia)) +", "  geom_point()"],
      ["ggplot(ele, aes(x = horas, y = nota_dele,", "                colour = estancia)) +", "  geom_point() +", "  geom_smooth(method = \"lm\", se = FALSE)"],
      ["ggplot(ele, aes(x = horas, y = nota_dele,", "                colour = estancia)) +", "  geom_point() +", "  geom_smooth(method = \"lm\", se = FALSE) +", "  facet_wrap(~ nivel, nrow = 1)"],
      ["ggplot(ele, aes(x = horas, y = nota_dele,", "                colour = estancia)) +", "  geom_point() +", "  geom_smooth(method = \"lm\", se = FALSE) +", "  facet_wrap(~ nivel, nrow = 1) +", "  labs(x = \"Horas de instrucción\",", "       y = \"Nota DELE\", colour = \"Estancia\")"]];
    const nuevas = [[0], [0], [1], [0, 1], [3], [4], [5, 6]];
    const expl = ["Solo datos: un lienzo vacío.", "<b>aes()</b> asigna variables a los ejes.", "<b>geom_point()</b>: una capa de puntos.",
      "Una tercera estética: el <b>color</b> según la estancia.", "Otra capa: una <b>recta</b> por grupo. <b>lm</b> = <i>linear model</i>: la recta que pasa más cerca de los puntos. <b>se = FALSE</b> quita la banda gris de incertidumbre.", "<b>Facetas</b>: un panel por nivel.", "<b>Etiquetas</b> legibles: listo para publicar."];
    const cod = crear(panel, "w-code", "pre"), rd = lectura(panel); panel.prepend(cod);
    const colE = { no: COL.azul, "sí": COL.rojo }, gEj = svg.append("g"), gP = svg.append("g"), gL = svg.append("g"), gT = svg.append("g");
    svg.append("rect").attr("x", 60).attr("y", 15).attr("width", 785).attr("height", 400).attr("fill", "#f5f7fa").lower();
    let paso = 0; const xr = d3.extent(D, d => d.horas), yr = [20, 100];
    function dibuja() {
      const fac = paso >= 5, paneles = fac ? NIVELES : ["todo"], ancho = 785 / paneles.length;
      const xs = paneles.map((p, i) => d3.scaleLinear(xr, [60 + i * ancho + 18, 60 + (i + 1) * ancho - 10]));
      const y = d3.scaleLinear(yr, [400, 25]); const pi = d => (fac ? NIVELES.indexOf(d.nivel) : 0);
      gEj.selectAll("*").remove();
      if (paso >= 1) {
        paneles.forEach((p, i) => { const g = gEj.append("g"); ejes(g, xs[i], i === 0 ? y : null, { y0: 415, x0: 60, tx: fac ? 3 : 6, sinY: i > 0 });
          if (fac) g.append("text").attr("class", "w-tag fuerte").attr("x", (xs[i].range()[0] + xs[i].range()[1]) / 2).attr("y", 12).attr("text-anchor", "middle").text(p); });
        gEj.append("text").attr("class", "eje-t").attr("x", 450).attr("y", 462).text(paso >= 6 ? "Horas de instrucción" : "horas");
        gEj.append("text").attr("class", "eje-t").attr("transform", "rotate(-90)").attr("x", -215).attr("y", 18).text(paso >= 6 ? "Nota DELE" : "nota_dele");
      }
      gP.selectAll("circle").data(D, d => d.id).join("circle").attr("r", 4.5)
        .transition().duration(800).attr("cx", d => xs[pi(d)](d.horas)).attr("cy", d => y(d.nota_dele))
        .attr("opacity", paso >= 2 ? 0.65 : 0).attr("fill", d => (paso >= 3 ? colE[d.estancia] : COL.tinta));
      const lineas = [];
      if (paso >= 4) paneles.forEach((p, i) => ["no", "sí"].forEach(e => {
        const sub = D.filter(d => d.estancia === e && (!fac || d.nivel === p)); const hx = sub.map(d => d.horas), ny = sub.map(d => d.nota_dele);
        const b1 = d3.sum(hx, (v, k) => (v - media(hx)) * (ny[k] - media(ny))) / d3.sum(hx, v => (v - media(hx)) ** 2), b0 = media(ny) - b1 * media(hx);
        const [a, b] = d3.extent(hx); lineas.push({ k: fac ? p + e : e, e, x1: xs[i](a), x2: xs[i](b), y1: y(b0 + b1 * a), y2: y(b0 + b1 * b) });
      }));
      gL.selectAll("line").data(lineas, d => d.k).join(en => en.append("line").attr("opacity", 0)).attr("stroke", d => colE[d.e]).attr("stroke-width", 4)
        .transition().duration(800).attr("opacity", 1).attr("x1", d => d.x1).attr("x2", d => d.x2).attr("y1", d => d.y1).attr("y2", d => d.y2);
      gT.selectAll("*").remove();
      if (paso >= 3) ["no", "sí"].forEach((e, i) => { gT.append("circle").attr("cx", 760 + i * 50).attr("cy", 470).attr("r", 7).attr("fill", colE[e]); gT.append("text").attr("class", "w-tag").attr("x", 770 + i * 50).attr("y", 475).text(e); });
      if (paso >= 3) gT.append("text").attr("class", "w-tag").attr("x", 750).attr("y", 475).attr("text-anchor", "end").text(paso >= 6 ? "Estancia" : "estancia");
      cod.innerHTML = pasos[paso].map((l, i) => `<span class="${nuevas[paso].includes(i) ? "nueva" : ""}">${l}</span>`).join("\n");
      rd.innerHTML = `<b>Capa ${paso + 1} de 7.</b> ${expl[paso]}`;
    }
    botones(panel, [{ etq: "← Quitar capa", cls: "sec", click: () => { paso = Math.max(0, paso - 1); dibuja(); } },
      { etq: "Añadir capa →", click: () => { paso = Math.min(6, paso + 1); dibuja(); } }]);
    dibuja();
    return { enter() { paso = 0; dibuja(); } };
  };

  W.histograma = el => {
    const D = CUR().ele, { vis, panel } = marco(el), w = 860, h = 460, svg = lienzo(vis, w, h);
    const st = { v: "nota_dele", bw: 5 }, gEj = svg.append("g"), gB = svg.append("g"); const cod = crear(panel, "w-code", "pre");
    const rango = { nota_dele: [1, 20, 5], horas: [5, 120, 25] }; let desl, rdH;
    function dibuja() {
      const vals = D.map(d => d[st.v]), ext = d3.extent(vals), x = d3.scaleLinear([ext[0] - st.bw, ext[1] + st.bw], [70, 830]);
      const th = d3.range(Math.floor(ext[0] / st.bw) * st.bw, ext[1] + st.bw, st.bw);
      const bins = d3.bin().domain([th[0], th[th.length - 1]]).thresholds(th)(vals), y = d3.scaleLinear([0, d3.max(bins, b => b.length)], [400, 20]).nice();
      ejes(gEj, x, y, { y0: 400, xlab: st.v, ylab: "Número de estudiantes" });
      gB.selectAll("rect").data(bins).join(en => en.append("rect").attr("y", 400).attr("height", 0)).attr("fill", COL.azul).attr("stroke", "#fff")
        .transition().duration(500).attr("x", b => x(b.x0)).attr("width", b => Math.max(0, x(b.x1) - x(b.x0))).attr("y", b => y(b.length)).attr("height", b => 400 - y(b.length));
      cod.innerHTML = `ggplot(ele, aes(x = <b>${st.v}</b>)) +\n  geom_histogram(binwidth = <b>${st.bw}</b>,\n                 boundary = 0,\n                 fill = "#1D3557",\n                 colour = "white")`;
      rdH.innerHTML = `Cada barra agrupa <b>${st.bw}</b> ${st.v === "nota_dele" ? (st.bw === 1 ? "punto de nota" : "puntos de nota") : "horas de clase"}: ${th[Math.floor(th.length / 2)]}–${th[Math.floor(th.length / 2)] + st.bw}, ${th[Math.floor(th.length / 2)] + st.bw}–${th[Math.floor(th.length / 2)] + 2 * st.bw}… · ${bins.length} barras`;
    }
    selector(panel, "Variable", ["nota_dele", "horas"], st.v, v => { st.v = v; const r = rango[v]; desl.inp.min = r[0]; desl.inp.max = r[1]; st.bw = r[2]; desl.set(r[2], true); dibuja(); });
    desl = deslizador(panel, { etq: "Anchura de la barra (binwidth)", min: 1, max: 20, valor: 5, cambio: v => { st.bw = v; dibuja(); } });
    rdH = lectura(panel); panel.appendChild(cod); crear(panel, "w-nota").innerHTML = "Barras muy finas: ruido. Barras muy anchas: se pierde la forma. No hay una anchura «correcta»: prueba varias.";
    dibuja();
  };

  W.geoms = el => {
    const D = CUR().ele, { vis, panel } = marco(el), w = 860, h = 470, svg = lienzo(vis, w, h);
    const st = { g: "nivel", puntos: true, caja: true, violin: false }, gEj = svg.append("g"), gV = svg.append("g"), gC = svg.append("g"), gP = svg.append("g");
    const y = d3.scaleLinear([20, 100], [420, 20]); const cod = crear(panel, "w-code", "pre");
    function dibuja() {
      const niv = st.g === "L1" ? L1S : NIVELES, x = d3.scaleBand(niv, [80, 840]).padding(0.25);
      ejes(gEj, x, y, { ylab: "nota_dele", xlab: st.g });
      const res = niv.map(k => { const a = D.filter(d => d[st.g] === k).map(d => d.nota_dele).sort(d3.ascending);
        const q1 = d3.quantile(a, .25), q2 = d3.median(a), q3 = d3.quantile(a, .75), r = q3 - q1;
        return { k, a, q1, q2, q3, lo: d3.min(a.filter(v => v >= q1 - 1.5 * r)), hi: d3.max(a.filter(v => v <= q3 + 1.5 * r)), dens: kde(a, [20, 100], silverman(a), 60) }; });
      const mx = d3.max(res, r => d3.max(r.dens, d => d[1])), xw = d3.scaleLinear([0, mx], [0, x.bandwidth() / 2]);
      const ar = d3.area().y(d => y(d[0])).x0(d => -xw(d[1])).x1(d => xw(d[1])).curve(d3.curveBasis);
      gV.selectAll("path").data(res, r => r.k).join("path").attr("fill", COL.agua).attr("stroke", COL.celeste)
        .attr("transform", r => `translate(${x(r.k) + x.bandwidth() / 2},0)`).attr("d", r => ar(r.dens))
        .transition().duration(500).attr("opacity", st.violin ? 0.8 : 0);
      const c = gC.selectAll("g").data(res, r => r.k).join(en => { const g = en.append("g"); g.append("line").attr("class", "b"); g.append("rect"); g.append("line").attr("class", "m"); return g; })
        .attr("transform", r => `translate(${x(r.k) + x.bandwidth() / 2},0)`);
      c.transition().duration(500).attr("opacity", st.caja ? 1 : 0);
      c.select(".b").attr("y1", r => y(r.lo)).attr("y2", r => y(r.hi)).attr("stroke", COL.tinta).attr("stroke-width", 2);
      c.select("rect").attr("x", -28).attr("width", 56).attr("y", r => y(r.q3)).attr("height", r => y(r.q1) - y(r.q3)).attr("fill", st.violin ? "#fff" : COL.agua).attr("stroke", COL.azul).attr("stroke-width", 2);
      c.select(".m").attr("x1", -28).attr("x2", 28).attr("y1", r => y(r.q2)).attr("y2", r => y(r.q2)).attr("stroke", COL.rojo).attr("stroke-width", 4);
      const r = rng(4);
      gP.selectAll("circle").data(D, d => d.id).join("circle").attr("r", 3.5).attr("fill", COL.azul)
        .transition().duration(600).attr("cx", d => x(d[st.g]) + x.bandwidth() / 2 + (r() - 0.5) * x.bandwidth() * 0.5).attr("cy", d => y(d.nota_dele)).attr("opacity", st.puntos ? 0.45 : 0);
      const capas = [st.violin && "  geom_violin(fill = \"#A8DADC\")", st.caja && `  geom_boxplot(${st.violin ? "width = .2" : "fill = \"#A8DADC\""})`, st.puntos && "  geom_jitter(width = .1, alpha = .4)"].filter(Boolean);
      cod.innerHTML = `ggplot(ele, aes(x = <b>${st.g}</b>, y = nota_dele))` + (capas.length ? " +\n" + capas.join(" +\n") : "");
    }
    segmentado(panel, "Grupos", ["nivel", "L1"], "nivel", v => { st.g = v; dibuja(); });
    const chk = crear(panel, "w-ctrl"); crear(chk, "w-lab").textContent = "Capas";
    [["puntos", "Puntos (jitter)"], ["caja", "Caja"], ["violin", "Violín"]].forEach(([k, t]) => {
      const lb = crear(chk, "w-chk", "label"), i = crear(lb, "", "input"); i.type = "checkbox"; i.checked = st[k]; lb.append(" " + t);
      i.onchange = () => { st[k] = i.checked; dibuja(); };
    });
    panel.appendChild(cod); dibuja();
  };

  W.heatmap = el => {
    const E = CUR().errores, { vis, panel } = marco(el), w = 880, h = 420, svg = lienzo(vis, w, h);
    const tipos = [...new Set(E.map(d => d.tipo))], x = d3.scaleBand(tipos, [130, 870]).padding(0.04), y = d3.scaleBand(L1S, [20, 380]).padding(0.04);
    const tot = d3.sum(E, d => d.n), fila = d3.rollup(E, v => d3.sum(v, d => d.n), d => d.L1), col = d3.rollup(E, v => d3.sum(v, d => d.n), d => d.tipo);
    E.forEach(d => { d.pf = d.n / fila.get(d.L1); const e = fila.get(d.L1) * col.get(d.tipo) / tot; d.res = (d.n - e) / Math.sqrt(e); });
    svg.append("g").selectAll("text").data(tipos).join("text").attr("class", "w-tag").attr("x", t => x(t) + x.bandwidth() / 2).attr("y", 405).attr("text-anchor", "middle").text(t => t);
    svg.append("g").selectAll("text").data(L1S).join("text").attr("class", "w-tag fuerte").attr("x", 120).attr("y", l => y(l) + y.bandwidth() / 2 + 5).attr("text-anchor", "end").text(l => l);
    const cel = svg.append("g").selectAll("g").data(E).join("g").attr("transform", d => `translate(${x(d.tipo)},${y(d.L1)})`);
    cel.append("rect").attr("width", x.bandwidth()).attr("height", y.bandwidth()).attr("rx", 6);
    const tx = cel.append("text").attr("x", x.bandwidth() / 2).attr("y", y.bandwidth() / 2 + 8).attr("text-anchor", "middle").attr("class", "celda");
    const rd = lectura(panel), cod = crear(panel, "w-code", "pre"); let modo = "n";
    const escalas = {
      n: d3.scaleSequential(d3.extent(E, d => d.n), d3.interpolateRgb(COL.agua, COL.azul)),
      pf: d3.scaleSequential(d3.extent(E, d => d.pf), d3.interpolateRgb(COL.agua, COL.rojo)),
      res: d3.scaleDiverging([-5, 0, 5], t => d3.interpolateRgbBasis([COL.rojo, "#ffffff", COL.verde])(t)) };
    const codigos = {
      n: "ggplot(errores, aes(tipo, L1, fill = n)) +\n  geom_tile(colour = \"white\") +\n  geom_text(aes(label = n))",
      pf: "errores |> group_by(L1) |>\n  mutate(pct = 100 * n / sum(n)) |>\n  ggplot(aes(tipo, L1, fill = pct)) +\n  geom_tile(colour = \"white\")",
      res: "m <- xtabs(n ~ L1 + tipo, errores)\nchisq.test(m)$residuals   # (O − E) / √E" };
    function dibuja() {
      const v = d => d[modo], s = escalas[modo];
      cel.select("rect").transition().duration(700).attr("fill", d => s(v(d)));
      tx.text(d => modo === "n" ? d.n : modo === "pf" ? f(100 * d.pf, 0) : f(d.res, 1))
        .attr("fill", d => modo === "res" ? (Math.abs(d.res) > 3 ? "#fff" : COL.tinta) : "#fff");
      cod.textContent = codigos[modo];
      rd.innerHTML = { n: "Recuentos brutos: el chino tiene más errores en todo… porque hay más textos.", pf: "Porcentaje por fila: comparamos <b>perfiles</b> de error.",
        res: "Residuos: <b style='color:#2A9D8F'>verde</b> = más de lo esperado, <b style='color:#E63946'>rojo</b> = menos. Lo más llamativo: <b>artículos</b> en chino." }[modo];
    }
    cel.on("mouseenter", (e, d) => { rd.innerHTML = `<b>${d.L1} · ${d.tipo}</b><br>${d.n} errores · ${pct(d.pf)} de su fila · residuo ${f(d.res, 1)}`; });
    segmentado(panel, "Mostrar", [["n", "Recuentos"], ["pf", "% por fila"], ["res", "Residuos"]], "n", v => { modo = v; dibuja(); });
    panel.appendChild(rd); panel.appendChild(cod); dibuja();
  };

  W["elige-grafico"] = el => {
    const D = CUR().ele, { vis, panel } = marco(el), w = 760, h = 420, svg = lienzo(vis, w, h); const st = { a: "num", b: "nada" };
    const fichas = {
      "num-nada": ["Histograma o densidad", "ggplot(ele, aes(nota_dele)) +\n  geom_histogram(binwidth = 5)"],
      "cat-nada": ["Gráfico de barras", "ggplot(ele, aes(L1)) +\n  geom_bar()"],
      "num-cat": ["Boxplot o violín (+ puntos)", "ggplot(ele, aes(L1, nota_dele)) +\n  geom_boxplot() + geom_jitter(width = .1)"],
      "num-num": ["Gráfico de dispersión", "ggplot(ele, aes(horas, nota_dele)) +\n  geom_point() + geom_smooth(method = \"lm\")"],
      "cat-cat": ["Barras apiladas al 100 % o mosaico", "ggplot(ele, aes(L1, fill = subjuntivo)) +\n  geom_bar(position = \"fill\")"],
      "cat-tabla": ["Mapa de calor", "ggplot(errores, aes(tipo, L1, fill = n)) +\n  geom_tile()"] };
    const tit = crear(panel, "w-read grande"), cod = crear(panel, "w-code", "pre");
    function dibuja() {
      let k = `${st.a}-${st.b}`; if (k === "cat-num") k = "num-cat"; svg.selectAll("*").remove();
      const [t, c] = fichas[k]; tit.innerHTML = `<b>${t}</b>`; cod.textContent = c;
      const g = svg.append("g"), y0 = 380;
      if (k === "num-nada") { const x = d3.scaleLinear([20, 100], [60, 720]), b = d3.bin().domain(x.domain()).thresholds(16)(D.map(d => d.nota_dele)), y = d3.scaleLinear([0, d3.max(b, v => v.length)], [y0, 30]);
        ejes(g, x, y, { y0, xlab: "nota_dele" }); g.selectAll("rect").data(b).join("rect").attr("x", v => x(v.x0)).attr("width", v => x(v.x1) - x(v.x0) - 1).attr("y", v => y(v.length)).attr("height", v => y0 - y(v.length)).attr("fill", COL.azul); }
      if (k === "cat-nada") { const c2 = d3.rollups(D, v => v.length, d => d.L1), x = d3.scaleBand(L1S, [60, 720]).padding(.3), y = d3.scaleLinear([0, 80], [y0, 30]);
        ejes(g, x, y, { y0, xlab: "L1" }); g.selectAll("rect").data(c2).join("rect").attr("x", v => x(v[0])).attr("width", x.bandwidth()).attr("y", v => y(v[1])).attr("height", v => y0 - y(v[1])).attr("fill", v => COL_L1[v[0]]); }
      if (k === "num-cat") { const x = d3.scaleBand(L1S, [60, 720]).padding(.3), y = d3.scaleLinear([20, 100], [y0, 30]); ejes(g, x, y, { y0, xlab: "L1", ylab: "nota_dele" });
        L1S.forEach(l => { const a = D.filter(d => d.L1 === l).map(d => d.nota_dele).sort(d3.ascending), q = [.25, .5, .75].map(p => d3.quantile(a, p));
          g.append("rect").attr("x", x(l)).attr("width", x.bandwidth()).attr("y", y(q[2])).attr("height", y(q[0]) - y(q[2])).attr("fill", COL.agua).attr("stroke", COL.azul);
          g.append("line").attr("x1", x(l)).attr("x2", x(l) + x.bandwidth()).attr("y1", y(q[1])).attr("y2", y(q[1])).attr("stroke", COL.rojo).attr("stroke-width", 3); }); }
      if (k === "num-num") { const x = d3.scaleLinear(d3.extent(D, d => d.horas), [60, 720]), y = d3.scaleLinear([20, 100], [y0, 30]); ejes(g, x, y, { y0, xlab: "horas", ylab: "nota_dele" });
        g.selectAll("circle").data(D).join("circle").attr("cx", d => x(d.horas)).attr("cy", d => y(d.nota_dele)).attr("r", 3.5).attr("fill", COL.azul).attr("opacity", .5); }
      if (k === "cat-cat") { const x = d3.scaleBand(L1S, [60, 720]).padding(.3), y = d3.scaleLinear([0, 1], [y0, 30]); ejes(g, x, y, { y0, xlab: "L1", fy: d3.format(".0%") });
        L1S.forEach(l => { const a = D.filter(d => d.L1 === l), p = a.filter(d => d.subjuntivo === "correcto").length / a.length;
          g.append("rect").attr("x", x(l)).attr("width", x.bandwidth()).attr("y", y(1)).attr("height", y(p) - y(1)).attr("fill", COL.rojo);
          g.append("rect").attr("x", x(l)).attr("width", x.bandwidth()).attr("y", y(p)).attr("height", y0 - y(p)).attr("fill", COL.verde); }); }
      if (k === "cat-tabla") { const E = CUR().errores, tipos = [...new Set(E.map(d => d.tipo))], x = d3.scaleBand(tipos, [60, 720]), y = d3.scaleBand(L1S, [30, y0]), c3 = d3.scaleSequential(d3.extent(E, d => d.n), d3.interpolateRgb(COL.agua, COL.azul));
        g.selectAll("rect").data(E).join("rect").attr("x", d => x(d.tipo)).attr("y", d => y(d.L1)).attr("width", x.bandwidth() - 2).attr("height", y.bandwidth() - 2).attr("fill", d => c3(d.n)); }
    }
    selector(panel, "Variable 1", [["num", "Numérica (nota, horas)"], ["cat", "Categórica (L1, nivel)"]], "num", v => { st.a = v; if (v === "num" && st.b === "tabla") st.b = "nada"; dibuja(); });
    selector(panel, "Variable 2", [["nada", "Ninguna"], ["num", "Numérica"], ["cat", "Categórica"], ["tabla", "Categórica con muchas celdas (tabla grande)"]], "nada", v => { st.b = v; if (v === "tabla") st.a = "cat"; dibuja(); });
    panel.appendChild(tit); panel.appendChild(cod); dibuja();
  };

  /* =====================================================================
     SESIÓN 3 · INFERENCIA
     ===================================================================== */

  W.normal = el => {
    const D = CUR().ele.map(d => d.nota_dele), { vis, panel } = marco(el), w = 860, h = 440, svg = lienzo(vis, w, h);
    const st = { m: 68, s: 12, z: 1.96, hist: false }, x = d3.scaleLinear([20, 120], [50, 830]), y = d3.scaleLinear([0, 0.09], [390, 20]);
    ejes(svg.append("g"), x, null, { y0: 390, sinY: true, xlab: "Nota" });
    const gH = svg.append("g"), sombra = svg.append("path").attr("fill", COL.agua).attr("opacity", 0.8), curva = svg.append("path").attr("fill", "none").attr("stroke", COL.azul).attr("stroke-width", 3.5);
    const gT = svg.append("g"); const rd = lectura(panel, "grande");
    function dibuja() {
      const xs = d3.range(20, 120.25, 0.25), lo = st.m - st.z * st.s, hi = st.m + st.z * st.s;
      const ln = d3.line().x(v => x(v)).y(v => y(Math.min(0.09, dnorm(v, st.m, st.s))));
      curva.transition().duration(300).attr("d", ln(xs));
      sombra.transition().duration(300).attr("d", d3.area().x(v => x(v)).y0(390).y1(v => y(Math.min(0.09, dnorm(v, st.m, st.s))))(xs.filter(v => v >= lo && v <= hi)));
      const p = 2 * pnorm(st.z) - 1;
      gT.selectAll("*").remove();
      gT.append("text").attr("x", x(st.m)).attr("y", 370).attr("text-anchor", "middle").attr("class", "w-tag fuerte grande").text(pct(p, 0));
      [lo, hi].forEach(v => gT.append("text").attr("x", x(v)).attr("y", 410).attr("text-anchor", "middle").attr("class", "w-tag rojo").text(f(v, 1)));
      const b = d3.bin().domain([20, 100]).thresholds(d3.range(20, 101, 4))(D);
      gH.selectAll("rect").data(st.hist ? b : []).join("rect").attr("x", v => x(v.x0)).attr("width", v => x(v.x1) - x(v.x0) - 1)
        .attr("y", v => y(v.length / D.length / 4)).attr("height", v => 390 - y(v.length / D.length / 4)).attr("fill", COL.arena).attr("opacity", 0.5);
      rd.innerHTML = `El <b>${pct(p, 0)}</b> de los valores cae entre <b>${f(lo, 1)}</b> y <b>${f(hi, 1)}</b> (media ± ${f(st.z, 2)} DE).`;
    }
    deslizador(panel, { etq: "Media (μ)", min: 40, max: 100, valor: 68, cambio: v => { st.m = v; dibuja(); } });
    deslizador(panel, { etq: "Desviación típica (σ)", min: 4, max: 20, valor: 12, cambio: v => { st.s = v; dibuja(); } });
    segmentado(panel, "Intervalo", [["1", "±1 DE"], ["1.96", "±1,96 DE"], ["2.576", "±2,58 DE"]], "1.96", v => { st.z = +v; dibuja(); });
    const lb = crear(crear(panel, "w-ctrl"), "w-chk", "label"), i = crear(lb, "", "input"); i.type = "checkbox"; lb.append(" Superponer las notas reales del DELE");
    i.onchange = () => { st.hist = i.checked; if (i.checked) { st.m = 68; st.s = 12; } dibuja(); };
    panel.appendChild(rd); dibuja();
  };

  W.muestral = el => {
    const P = CUR().ele.map(d => d.nota_dele), { vis, panel } = marco(el), w = 880, h = 500, svg = lienzo(vis, w, h);
    const mu = media(P), sigma = Math.sqrt(d3.variance(P) * (P.length - 1) / P.length);
    const xt = d3.scaleLinear([20, 100], [60, 840]), xb = d3.scaleLinear([50, 86], [60, 840]);
    const binsP = d3.bin().domain([20, 100]).thresholds(d3.range(20, 101, 2))(P), yt = d3.scaleLinear([0, d3.max(binsP, b => b.length)], [170, 30]);
    svg.append("text").attr("class", "w-tag").attr("x", 60).attr("y", 18).text("Población (240 notas)");
    svg.append("g").selectAll("rect").data(binsP).join("rect").attr("x", b => xt(b.x0)).attr("width", b => xt(b.x1) - xt(b.x0) - 1).attr("y", b => yt(b.length)).attr("height", b => 170 - yt(b.length)).attr("fill", "#cdd6e0");
    ejes(svg.append("g"), xt, null, { y0: 170, sinY: true });
    const gS = svg.append("g");
    svg.append("text").attr("class", "w-tag").attr("x", 60).attr("y", 232).text("Distribución muestral: una bolita por cada media");
    ejes(svg.append("g"), xb, null, { y0: 470, sinY: true, tx: 12 });
    svg.append("line").attr("x1", xb(mu)).attr("x2", xb(mu)).attr("y1", 240).attr("y2", 470).attr("stroke", COL.rojo).attr("stroke-width", 2).attr("stroke-dasharray", "5 4");
    const gM = svg.append("g"), curva = svg.append("path").attr("fill", "none").attr("stroke", COL.rojo).attr("stroke-width", 2.5);
    let n = 30, medias = []; const rd = lectura(panel, "grande");
    function redibuja() {
      const paso = 0.6, cnt = {};
      const pos = medias.map(m => { const k = Math.round(m / paso); cnt[k] = (cnt[k] || 0) + 1; return { x: xb(k * paso), c: cnt[k] }; });
      const maxc = d3.max(pos, p => p.c) || 1, dy = Math.min(9, 215 / maxc);
      gM.selectAll("circle").data(pos).join("circle").attr("r", Math.min(4.5, dy / 2)).attr("fill", COL.celeste).attr("cx", p => p.x).attr("cy", p => 466 - (p.c - 1) * dy);
      const ee = sigma / Math.sqrt(n), escala = medias.length * paso * dy;
      curva.attr("d", medias.length > 20 ? d3.line()(d3.range(50, 86, 0.25).map(v => [xb(v), 466 - dnorm(v, mu, ee) * escala])) : null);
      rd.innerHTML = `<div>${medias.length} muestras de n = ${n}</div><div>Media de las medias <b>${medias.length ? f(media(medias), 2) : "—"}</b></div>
        <div>DE de las medias <b>${medias.length > 2 ? f(de(medias), 2) : "—"}</b></div><div>Error estándar teórico σ/√n = ${f(sigma, 1)} / √${n} = <b class="rojo">${f(ee, 2)}</b></div><div class="peq">σ = DE de las 240 notas; n = tamaño de cada muestra. Las medias suelen alejarse unos ${f(ee, 1)} puntos de la media real.</div>`;
    }
    function una(anim) {
      const s = d3.range(n).map(() => P[Math.floor(Math.random() * P.length)]), m = media(s);
      if (anim) {
        gS.selectAll("circle").data(s).join("circle").attr("r", 5).attr("fill", COL.rojo).attr("cx", v => xt(v)).attr("cy", 190).attr("opacity", 1)
          .transition().duration(600).attr("cx", xb(Math.max(50, Math.min(86, m)))).attr("cy", 300).attr("opacity", 0).on("end", (d, i) => { if (i === 0) { medias.push(m); redibuja(); } });
      } else medias.push(m);
    }
    deslizador(panel, { etq: "Tamaño de cada muestra (n)", min: 2, max: 100, valor: 30, cambio: v => { n = v; medias = []; redibuja(); } });
    botones(panel, [{ etq: "1 muestra", click: () => una(true) }, { etq: "+ 100", cls: "sec", click: () => { for (let i = 0; i < 100; i++) una(false); redibuja(); } },
      { etq: "+ 1000", cls: "sec", click: () => { for (let i = 0; i < 1000; i++) una(false); redibuja(); } }, { etq: "Reiniciar", cls: "ter", click: () => { medias = []; redibuja(); } }]);
    panel.appendChild(rd); redibuja();
  };

  W.ic = el => {
    const P = CUR().ele.map(d => d.nota_dele), mu = media(P), { vis, panel } = marco(el), w = 880, h = 480, svg = lienzo(vis, w, h);
    const st = { n: 30, c: 0.95, k: 50 }, x = d3.scaleLinear([0.5, 50.5], [50, 860]), y = d3.scaleLinear([50, 86], [440, 20]);
    ejes(svg.append("g"), x, y, { y0: 440, xlab: "Muestra", ylab: "IC de la media" , tx: 10});
    svg.append("line").attr("x1", 50).attr("x2", 860).attr("y1", y(mu)).attr("y2", y(mu)).attr("stroke", COL.tinta).attr("stroke-dasharray", "6 4").attr("stroke-width", 2);
    svg.append("text").attr("x", 58).attr("y", 36).attr("class", "w-tag fuerte").text("- - - media real de la población: " + f(mu, 1));
    const g = svg.append("g"); const rd = lectura(panel, "grande");
    function nuevas() {
      const tq = qt(1 - (1 - st.c) / 2, st.n - 1);
      const ics = d3.range(st.k).map(i => { const s = d3.range(st.n).map(() => P[Math.floor(Math.random() * P.length)]), m = media(s), e = de(s) / Math.sqrt(st.n);
        return { i, m, lo: m - tq * e, hi: m + tq * e, ok: m - tq * e <= mu && m + tq * e >= mu }; });
      const s = g.selectAll("g").data(ics).join(en => { const gg = en.append("g"); gg.append("line"); gg.append("circle"); return gg; });
      s.attr("transform", d => `translate(${x(d.i + 1)},0)`).attr("opacity", 0).transition().delay(d => d.i * 25).duration(200).attr("opacity", 1);
      s.select("line").attr("y1", d => y(Math.max(50, d.lo))).attr("y2", d => y(Math.min(86, d.hi))).attr("stroke", d => d.ok ? COL.celeste : COL.rojo).attr("stroke-width", d => d.ok ? 3 : 5);
      s.select("circle").attr("cy", d => y(d.m)).attr("r", 4.5).attr("fill", d => d.ok ? COL.azul : COL.rojo);
      const k = ics.filter(d => d.ok).length, semi = d3.mean(ics, d => (d.hi - d.lo) / 2);
      rd.innerHTML = `<div><b>${k} de ${st.k}</b> intervalos (${pct(k / st.k)}) contienen la media real.</div><div class="rojo">${st.k - k} fallan.</div>
        <div>Anchura media: <b>± ${f(semi, 1)}</b> puntos</div>
        <div class="peq">Nivel de confianza ${pct(st.c)} · n = ${st.n} · cada intervalo = media ± ${f(tq, 2)} × DE / √${st.n}</div>`;
    }
    deslizador(panel, { etq: "Tamaño de la muestra (n)", min: 5, max: 120, valor: 30, cambio: v => { st.n = v; nuevas(); } });
    segmentado(panel, "Confianza", [["0.8", "80 %"], ["0.9", "90 %"], ["0.95", "95 %"], ["0.99", "99 %"]], "0.95", v => { st.c = +v; nuevas(); });
    botones(panel, [{ etq: "Nuevas 50 muestras", click: nuevas }]); panel.appendChild(rd); nuevas();
  };

  W["valor-p"] = el => {
    const { vis, panel } = marco(el), w = 860, h = 420, svg = lienzo(vis, w, h), df = Math.round(CUR().ttest.gl);
    const st = { t: 2.2, colas: 2 }, x = d3.scaleLinear([-5, 5], [50, 830]), y = d3.scaleLinear([0, 0.42], [370, 20]);
    ejes(svg.append("g"), x, null, { y0: 370, sinY: true, xlab: "Valores de t si H₀ fuera cierta" });
    const xs = d3.range(-5, 5.01, 0.02), ar = d3.area().x(v => x(v)).y0(370).y1(v => y(dt(v, df)));
    svg.append("path").attr("d", d3.line().x(v => x(v)).y(v => y(dt(v, df)))(xs)).attr("fill", "none").attr("stroke", COL.azul).attr("stroke-width", 3.5);
    const cDer = svg.append("path").attr("fill", COL.rojo).attr("opacity", 0.75), cIzq = svg.append("path").attr("fill", COL.rojo).attr("opacity", 0.75);
    const linea = svg.append("line").attr("y1", 30).attr("y2", 370).attr("stroke", COL.tinta).attr("stroke-width", 2.5).attr("stroke-dasharray", "6 4");
    const et = svg.append("text").attr("y", 26).attr("class", "w-tag fuerte").attr("text-anchor", "middle");
    const crit = svg.append("g"); const rd = lectura(panel, "grande");
    function dibuja() {
      const t = st.t, p = st.colas === 2 ? pt2(t, df) : pt2(t, df) / 2;
      cDer.attr("d", ar(xs.filter(v => v >= t))); cIzq.attr("d", st.colas === 2 ? ar(xs.filter(v => v <= -t)) : null);
      linea.attr("x1", x(t)).attr("x2", x(t)); et.attr("x", x(t)).text("t = " + f(t, 2));
      const tc = qt(st.colas === 2 ? 0.975 : 0.95, df);
      crit.selectAll("line").data(st.colas === 2 ? [-tc, tc] : [tc]).join("line").attr("x1", x).attr("x2", x).attr("y1", 300).attr("y2", 385).attr("stroke", COL.arena).attr("stroke-width", 3);
      crit.selectAll("text").data(st.colas === 2 ? [-tc, tc] : [tc]).join("text").attr("x", x).attr("y", 402).attr("text-anchor", "middle").attr("class", "w-tag").attr("fill", COL.arena).text(v => "valor crítico " + f(v, 2));
      const sig = p < 0.05;
      rd.innerHTML = `<div>p ${fp(p)}</div><div class="${sig ? "verde" : "rojo"}"><b>${sig ? "Rechazamos H₀" : "No podemos rechazar H₀"}</b></div>
        <div class="peq">Área roja = probabilidad de obtener un t tan extremo o más si H₀ fuera cierta (gl = ${df}).</div><div class="peq">${st.colas === 2 ? "<b>Bilateral</b>: H₁ = «las medias son distintas» (en cualquier dirección): se suman las dos colas." : "<b>Unilateral</b>: H₁ = «la media con estancia es mayor»: solo cuenta una cola, así que p es la mitad."}</div>`;
    }
    const s = deslizador(panel, { etq: "t observado", min: 0, max: 5, paso: 0.01, valor: 2.2, fmt: v => f(v, 2), cambio: v => { st.t = v; dibuja(); } });
    segmentado(panel, "Contraste", [["2", "Bilateral"], ["1", "Unilateral"]], "2", v => { st.colas = +v; dibuja(); });
    botones(panel, [{ etq: "Nuestro t (estancia)", click: () => s.set(Math.abs(CUR().ttest.t)) }]);
    panel.appendChild(rd); dibuja();
  };

  W.potencia = el => {
    const { vis, panel } = marco(el), w = 860, h = 420, svg = lienzo(vis, w, h), st = { d: 0.5, n: 30, a: 0.05 };
    const x = d3.scaleLinear([-4, 8], [40, 840]), y = d3.scaleLinear([0, 0.42], [360, 20]);
    ejes(svg.append("g"), x, null, { y0: 360, sinY: true, xlab: "Estadístico de contraste (z)" });
    const aB = svg.append("path").attr("fill", COL.arena).attr("opacity", .6), aP = svg.append("path").attr("fill", COL.verde).attr("opacity", .45), aA = svg.append("path").attr("fill", COL.rojo).attr("opacity", .8);
    const c0 = svg.append("path").attr("fill", "none").attr("stroke", COL.azul).attr("stroke-width", 3), c1 = svg.append("path").attr("fill", "none").attr("stroke", COL.verde).attr("stroke-width", 3);
    const lc = svg.append("line").attr("y1", 20).attr("y2", 360).attr("stroke", COL.tinta).attr("stroke-dasharray", "6 4").attr("stroke-width", 2);
    const t0 = svg.append("text").attr("class", "w-tag fuerte").attr("fill", COL.azul).attr("text-anchor", "middle").attr("y", 14).text("H₀ cierta");
    const t1 = svg.append("text").attr("class", "w-tag fuerte").attr("fill", COL.verde).attr("text-anchor", "middle").attr("y", 14).text("H₁ cierta");
    const rd = lectura(panel, "grande");
    function dibuja() {
      const delta = st.d * Math.sqrt(st.n / 2), zc = qnorm(1 - st.a / 2), xs = d3.range(-4, 8.01, 0.02);
      const ar = m => d3.area().x(v => x(v)).y0(360).y1(v => y(dnorm(v, m)));
      c0.attr("d", d3.line().x(v => x(v)).y(v => y(dnorm(v, 0)))(xs)); c1.transition().duration(250).attr("d", d3.line().x(v => x(v)).y(v => y(dnorm(v, delta)))(xs));
      aA.attr("d", ar(0)(xs.filter(v => v >= zc))); aB.transition().duration(250).attr("d", ar(delta)(xs.filter(v => v <= zc))); aP.transition().duration(250).attr("d", ar(delta)(xs.filter(v => v >= zc)));
      lc.attr("x1", x(zc)).attr("x2", x(zc)); t0.attr("x", x(0)); t1.transition().duration(250).attr("x", x(delta));
      const potN = nn => { const dl = st.d * Math.sqrt(nn / 2); return 1 - pnorm(zc - dl) + pnorm(-zc - dl); }, pot = potN(st.n);
      let n80 = 2; while (potN(n80) < 0.8 && n80 < 2000) n80++;
      rd.innerHTML = `<div><span class="cuadro" style="background:${COL.rojo}"></span>α (falsa alarma) <b>${pct(st.a)}</b></div>
        <div><span class="cuadro" style="background:${COL.arena}"></span>β (se me escapa) <b>${pct(1 - pot)}</b></div>
        <div><span class="cuadro" style="background:${COL.verde}"></span>Potencia 1 − β <b>${pct(pot)}</b></div>
        <div class="peq">d = ${f(st.d, 2)} equivale a una diferencia real de unos <b>${f(st.d * 13, 0)} puntos</b> de nota DELE (DE ≈ 13).</div>
        <div class="peq">Con <b>${st.n}</b> por grupo, la potencia es del ${pct(pot)}. Para llegar al 80 % harían falta, como mínimo, unos <b>${st.d > 0.05 ? n80 : "∞"}</b> estudiantes por grupo.</div>`;
    }
    deslizador(panel, { etq: "Tamaño del efecto (d)", min: 0, max: 1.5, paso: 0.05, valor: 0.5, fmt: v => f(v, 2), cambio: v => { st.d = v; dibuja(); } });
    deslizador(panel, { etq: "n por grupo", min: 5, max: 200, valor: 30, cambio: v => { st.n = v; dibuja(); } });
    segmentado(panel, "α", [["0.01", "0,01"], ["0.05", "0,05"], ["0.1", "0,10"]], "0.05", v => { st.a = +v; dibuja(); });
    panel.appendChild(rd); dibuja();
  };

  W["cohen-d"] = el => {
    const D = CUR().ele, { vis, panel } = marco(el), w = 860, h = 312, svg = lienzo(vis, w, h);
    const x = d3.scaleLinear([-4, 6], [40, 840]), y = d3.scaleLinear([0, 0.42], [260, 40]); const xs = d3.range(-4, 6.01, 0.02);
    ejes(svg.append("g"), x, null, { y0: 260, sinY: true, xlab: "Nota DELE (puntos)", fx: v => f(65.9 + 12.7 * v, 0) });
    const etA = svg.append("text").attr("class", "w-tag fuerte").attr("fill", COL.azul).attr("text-anchor", "middle").attr("x", x(0)).attr("y", 30).text("sin estancia");
    const etB = svg.append("text").attr("class", "w-tag fuerte").attr("fill", COL.rojo).attr("text-anchor", "middle").attr("y", 50);
    const a1 = svg.append("path").attr("fill", COL.azul).attr("opacity", .35), a2 = svg.append("path").attr("fill", COL.rojo).attr("opacity", .35);
    const rd = lectura(panel, "grande");
    const est = welch(D.filter(d => d.estancia === "sí").map(d => d.nota_dele), D.filter(d => d.estancia === "no").map(d => d.nota_dele)).d;
    function dibuja(dd) {
      const ar = m => d3.area().x(v => x(v)).y0(260).y1(v => y(dnorm(v, m)))(xs);
      a1.attr("d", ar(0)); a2.transition().duration(300).attr("d", ar(dd)); etB.transition().duration(300).attr("x", x(dd)).text(dd ? "con estancia" : "");
      const ovl = 2 * pnorm(-Math.abs(dd) / 2), u3 = pnorm(dd), ps = pnorm(dd / Math.SQRT2);
      const et = Math.abs(dd) < 0.2 ? "despreciable" : Math.abs(dd) < 0.5 ? "pequeño" : Math.abs(dd) < 0.8 ? "mediano" : "grande";
      rd.innerHTML = `<div>d = <b>${f(dd, 2)}</b> → efecto <b>${et}</b></div><div>Diferencia: <b>${f(12.7 * dd, 1)} puntos</b> de nota (${f(dd, 2)} × DE de 12,7)</div><div>Solapamiento de las curvas <b>${pct(ovl)}</b></div>
        <div>El ${pct(u3)} de los que tienen estancia supera la nota media de los que no (sin efecto: 50 %)</div><div class="peq">Si elegimos al azar a uno de cada grupo, el que tiene estancia saca más nota el ${pct(ps)} de las veces (sin efecto: 50 %).</div>`;
    }
    const s = deslizador(panel, { etq: "d de Cohen", min: 0, max: 2.5, paso: 0.05, valor: 0.5, fmt: v => f(v, 2), cambio: dibuja });
    botones(panel, [{ etq: "Nuestra d (estancia)", click: () => s.set(Math.round(est * 100) / 100) }]);
    panel.appendChild(rd); dibuja(0.5);
  };

  /* --- Árbol para elegir la prueba estadística --- */
  const ARBOL_PRUEBAS = {
    q: "¿Qué tipo de variable quieres explicar (respuesta)?", c: [
      { r: "Numérica", q: "¿Qué tipo es tu predictor?", c: [
        { r: "Grupos (categórica)", q: "¿Cuántos grupos comparas?", c: [
          { r: "2 grupos", q: "¿Son las mismas personas medidas dos veces?", c: [
            { r: "No: grupos distintos", q: "¿Distribución aproximadamente normal (o n > 30 por grupo)?", c: [
              { r: "Sí", fin: "Prueba t de Welch", cod: "t.test(nota_dele ~ estancia, data = ele)", ef: "d de Cohen", ej: "¿Sacan más nota quienes han hecho una estancia?" },
              { r: "No", fin: "Wilcoxon (Mann-Whitney)", cod: "wilcox.test(err_ser_estar ~ estancia, data = ele)", ef: "r = Z / √N", ej: "¿Cometen menos errores de ser/estar con estancia?" }] },
            { r: "Sí: pretest/postest", q: "¿Diferencias aproximadamente normales?", c: [
              { r: "Sí", fin: "Prueba t para muestras relacionadas", cod: "t.test(post, pre, paired = TRUE)", ef: "d de las diferencias", ej: "¿Mejoran tras la intervención didáctica?" },
              { r: "No", fin: "Wilcoxon de rangos con signo", cod: "wilcox.test(post, pre, paired = TRUE)", ef: "r", ej: "Pretest/postest con pocos alumnos" }] }] },
          { r: "3 o más grupos", q: "¿Residuos normales y varianzas parecidas?", c: [
            { r: "Sí", fin: "ANOVA + Tukey", cod: "m <- aov(nota_dele ~ L1, data = ele)\nsummary(m); TukeyHSD(m)", ef: "η²", ej: "¿Difiere la nota según la L1?" },
            { r: "No", fin: "Kruskal-Wallis", cod: "kruskal.test(err_ser_estar ~ L1, data = ele)", ef: "ε²", ej: "¿Difieren los errores según la L1?" }] }] },
        { r: "Numérica", q: "¿Quieres medir la relación o predecir?", c: [
          { r: "Medir la relación", q: "¿Relación lineal y datos normales?", c: [
            { r: "Sí", fin: "Correlación de Pearson", cod: "cor.test(ele$horas, ele$nota_dele)", ef: "r y r²", ej: "¿A más horas, más nota?" },
            { r: "No", fin: "Correlación de Spearman", cod: "cor.test(ele$horas, ele$err_ser_estar,\n         method = \"spearman\")", ef: "ρ", ej: "Horas y número de errores" }] },
          { r: "Predecir", fin: "Regresión lineal (Plus) o árbol de regresión", cod: "lm(nota_dele ~ horas + estancia, data = ele)", ef: "R² y coeficientes", ej: "¿Cuánto sube la nota por hora de clase?" }] }] },
      { r: "Categórica", q: "¿Qué quieres saber?", c: [
        { r: "Si dos categóricas están asociadas", q: "¿Frecuencias esperadas ≥ 5 en todas las celdas?", c: [
          { r: "Sí", fin: "Chi cuadrado (χ²)", cod: "chisq.test(table(ele$estancia, ele$subjuntivo))", ef: "V de Cramér", ej: "¿Depende el subjuntivo de la estancia?" },
          { r: "No", fin: "Prueba exacta de Fisher", cod: "fisher.test(tabla)", ef: "odds ratio", ej: "Tablas con celdas pequeñas" }] },
        { r: "Predecirla con varios predictores", q: "¿Qué priorizas?", c: [
          { r: "Cuantificar cada efecto", fin: "Regresión logística (Plus)", cod: "glm(subj_ok ~ horas + estancia + L1,\n    data = ele, family = binomial)", ef: "odds ratios", ej: "¿Qué predice el uso correcto del subjuntivo?" },
          { r: "Reglas fáciles de explicar", fin: "Árbol de decisión", cod: "rpart(subjuntivo ~ horas + estancia + L1 + nivel,\n      data = ele, method = \"class\")", ef: "acierto y matriz de confusión", ej: "¿Qué perfil de estudiante usa bien el subjuntivo?" }] }] },
      { r: "Frecuencias de palabras en dos corpus", fin: "Log-likelihood + log-ratio (Plus)", cod: "ll(aprend, nativo, N1, N2)\nlog2((aprend / N1) / (nativo / N2))", ef: "log-ratio", ej: "¿Qué palabras sobreusan los estudiantes?" }] };

  W["elige-prueba"] = el => {
    const { vis, panel } = marco(el, { ancho: 470 }), w = 900, h = 560, svg = lienzo(vis, w, h);
    const raiz = d3.hierarchy(ARBOL_PRUEBAS, d => d.c); d3.tree().size([h - 40, w - 230])(raiz);
    raiz.each(d => { const t = d.x; d.x = d.y + 20; d.y = t + 20; });
    const link = d3.linkHorizontal().x(d => d.x).y(d => d.y);
    const gl = svg.append("g").selectAll("path").data(raiz.links()).join("path").attr("d", link).attr("fill", "none").attr("stroke", "#cfd8e3").attr("stroke-width", 2);
    const gn = svg.append("g").selectAll("g").data(raiz.descendants()).join("g").attr("transform", d => `translate(${d.x},${d.y})`).style("cursor", "pointer");
    gn.append("circle").attr("r", d => d.data.fin ? 8 : 6);
    gn.append("text").attr("x", 11).attr("y", 4).attr("class", "w-tag nodo-t").text(d => d.data.fin || (d.depth ? d.data.r : "Inicio"));
    const preg = crear(panel, "w-read grande"), opc = crear(panel, "w-opciones"), migas = crear(panel, "w-migas"), fin = crear(panel, "w-fin");
    let actual = raiz;
    function camino(n) { return n.ancestors().reverse(); }
    function dibuja() {
      const cam = new Set(camino(actual));
      gl.transition().duration(400).attr("stroke", l => cam.has(l.target) ? COL.rojo : "#dfe5ec").attr("stroke-width", l => cam.has(l.target) ? 4 : 1.5);
      gn.select("circle").transition().duration(400).attr("fill", d => d === actual ? COL.rojo : cam.has(d) ? COL.azul : d.data.fin ? COL.agua : "#cfd8e3").attr("r", d => d === actual ? 11 : d.data.fin ? 8 : 6);
      gn.select("text").attr("opacity", d => cam.has(d) || d.parent === actual ? 1 : 0.55).attr("font-weight", d => cam.has(d) ? 700 : 400);
      migas.innerHTML = camino(actual).slice(1).map(n => `<span>${n.data.r}</span>`).join(" › ");
      opc.innerHTML = ""; fin.innerHTML = "";
      if (actual.data.fin) {
        preg.innerHTML = `<span class="peq">Prueba recomendada</span><br><b class="rojo">${actual.data.fin}</b>`;
        fin.innerHTML = `<pre class="w-code">${actual.data.cod}</pre><div><b>Tamaño del efecto:</b> ${actual.data.ef}</div><div class="peq"><b>Ejemplo:</b> ${actual.data.ej}</div>`;
      } else {
        preg.innerHTML = actual.data.q;
        actual.children.forEach(c => { const b = crear(opc, "w-btn opcion", "button"); b.textContent = c.data.r; b.onclick = () => { actual = c; dibuja(); }; });
      }
    }
    gn.on("click", (e, d) => { actual = d; dibuja(); });
    botones(panel, [{ etq: "← Atrás", cls: "sec", click: () => { if (actual.parent) { actual = actual.parent; dibuja(); } } }, { etq: "Empezar de nuevo", cls: "ter", click: () => { actual = raiz; dibuja(); } }]);
    dibuja();
    return { enter() { actual = raiz; dibuja(); } };
  };

  W["senal-ruido"] = el => {
    const { vis, panel } = marco(el), w = 860, h = 440, svg = lienzo(vis, w, h), st = { dif: 7, s: 10, n: 40 };
    const za = d3.range(120).map(rnormGen(rng(21))), zb = d3.range(120).map(rnormGen(rng(22))), ja = d3.range(120).map(rng(5)), jb = d3.range(120).map(rng(6));
    const y = d3.scaleLinear([20, 120], [400, 20]), x = d3.scaleBand(["Grupo A", "Grupo B"], [80, 600]).padding(0.3);
    ejes(svg.append("g"), x, y, { y0: 400, ylab: "Nota" });
    const gA = svg.append("g"), gB = svg.append("g"), mA = svg.append("line"), mB = svg.append("line");
    const barra = svg.append("g").attr("transform", "translate(680,40)");
    barra.append("text").attr("class", "w-tag fuerte").attr("x", 40).attr("y", -10).attr("text-anchor", "middle").text("|t| observado");
    const rb = barra.append("rect").attr("x", 15).attr("width", 50).attr("fill", COL.rojo), lin = barra.append("line").attr("x1", 0).attr("x2", 80).attr("stroke", COL.tinta).attr("stroke-dasharray", "5 3");
    const tCrit = barra.append("text").attr("class", "w-tag").attr("x", 85);
    const rd = lectura(panel, "grande");
    function dibuja() {
      const a = za.slice(0, st.n).map(z => 65 + z * st.s), b = zb.slice(0, st.n).map(z => 65 + st.dif + z * st.s), r = welch(b, a);
      const cx = (g, k, j) => x(g) + x.bandwidth() / 2 + (j[k] - 0.5) * x.bandwidth() * 0.6;
      gA.selectAll("circle").data(a).join("circle").attr("r", 5).attr("fill", COL.azul).attr("opacity", .55).attr("cx", (v, k) => cx("Grupo A", k, ja)).transition().duration(300).attr("cy", v => y(Math.max(20, Math.min(120, v))));
      gB.selectAll("circle").data(b).join("circle").attr("r", 5).attr("fill", COL.rojo).attr("opacity", .55).attr("cx", (v, k) => cx("Grupo B", k, jb)).transition().duration(300).attr("cy", v => y(Math.max(20, Math.min(120, v))));
      [[mA, "Grupo A", r.mb], [mB, "Grupo B", r.ma]].forEach(([l, g, m]) => l.attr("x1", x(g)).attr("x2", x(g) + x.bandwidth()).attr("stroke", COL.tinta).attr("stroke-width", 5).transition().duration(300).attr("y1", y(m)).attr("y2", y(m)));
      const ys = d3.scaleLinear([0, 10], [340, 0]), tc = qt(0.975, r.gl);
      rb.transition().duration(300).attr("y", ys(Math.min(10, Math.abs(r.t)))).attr("height", 340 - ys(Math.min(10, Math.abs(r.t)))).attr("fill", r.p < 0.05 ? COL.verde : COL.rojo);
      lin.attr("y1", ys(tc)).attr("y2", ys(tc)); tCrit.attr("y", ys(tc) + 4).text("valor crítico " + f(tc, 2));
      const eeD = r.t ? (r.ma - r.mb) / r.t : NaN;
      rd.innerHTML = `<div>Señal (diferencia de medias) <b>${f(r.ma - r.mb, 1)}</b></div><div>Ruido (EE de la diferencia) <b>${f(eeD, 2)}</b> <span class="peq">con DE ${f(st.s, 1)} y n = ${st.n}</span></div>
        <div>t = ${f(r.ma - r.mb, 1)} / ${f(eeD, 2)} = <b>${f(r.t, 2)}</b> · gl = ${f(r.gl, 1)}</div><div>p ${fp(r.p)} · d = ${f(r.d, 2)}</div>
        <div class="peq">${Math.abs(r.t) > tc ? `|t| supera el valor crítico (${f(tc, 2)}): p < 0,05, diferencia significativa.` : `|t| no llega al valor crítico (${f(tc, 2)}): p > 0,05, podría ser azar.`}</div>`;
    }
    deslizador(panel, { etq: "Diferencia real entre grupos", min: 0, max: 15, paso: 0.5, valor: 7, fmt: v => f(v, 1), cambio: v => { st.dif = v; dibuja(); } });
    deslizador(panel, { etq: "Variabilidad dentro de los grupos (DE)", min: 2, max: 25, paso: 0.5, valor: 10, fmt: v => f(v, 1), cambio: v => { st.s = v; dibuja(); } });
    deslizador(panel, { etq: "n por grupo", min: 5, max: 120, valor: 40, cambio: v => { st.n = v; dibuja(); } });
    panel.appendChild(rd); dibuja();
  };

  W.wilcoxon = el => {
    const { vis, panel } = marco(el), w = 860, h = 380, svg = lienzo(vis, w, h);
    const A = [3, 5, 2, 6, 4, 8, 3, 12], B = [1, 2, 0, 3, 1, 4, 2, 1];
    const datos = A.map(v => ({ v, g: "no" })).concat(B.map(v => ({ v, g: "sí" }))), rk = rangos(datos.map(d => d.v));
    datos.forEach((d, i) => { d.r = rk[i]; });
    const orden = [...datos].sort((a, b) => a.v - b.v || (a.g < b.g ? -1 : 1)); orden.forEach((d, i) => { d.pos = i; });
    const xv = d3.scaleLinear([0, 13], [170, 830]), xr = d3.scaleLinear([0, 15], [80, 820]);
    const ax = svg.append("g"); let modo = 0; const rd = lectura(panel, "grande");
    const g = svg.append("g").selectAll("g").data(datos).join(en => { const gg = en.append("g"); gg.append("circle").attr("r", 18); gg.append("text").attr("class", "celda").attr("text-anchor", "middle").attr("y", 6); gg.append("text").attr("class", "rk w-tag fuerte").attr("text-anchor", "middle").attr("y", 44); return gg; });
    g.select("circle").attr("fill", d => d.g === "no" ? COL.azul : COL.rojo);
    g.select(".celda").text(d => d.v).attr("fill", "#fff");
    function dibuja() {
      const cnt = {};
      ax.selectAll("*").remove();
      if (!modo) { ejes(ax, xv, null, { y0: 320, sinY: true, xlab: "Errores de ser/estar" });
        ax.append("text").attr("class", "w-tag fuerte").attr("x", 20).attr("y", 105).attr("fill", COL.azul).text("sin estancia");
        ax.append("text").attr("class", "w-tag fuerte").attr("x", 20).attr("y", 235).attr("fill", COL.rojo).text("con estancia"); }
      else ax.append("text").attr("class", "w-tag").attr("x", 80).attr("y", 300).text("Todos juntos, ordenados de menor a mayor: debajo, el rango (los empates comparten rango medio)");
      g.transition().duration(1000).attr("transform", d => { if (modo) return `translate(${xr(d.pos)},${180})`;
        const k = d.g + d.v; cnt[k] = (cnt[k] || 0) + 1; return `translate(${xv(d.v)},${(d.g === "no" ? 100 : 230) - (cnt[k] - 1) * 40})`; });
      g.select(".rk").text(d => modo ? f(d.r, 1).replace(",0", "") : "").attr("opacity", modo ? 1 : 0);
      const sA = d3.sum(datos.filter(d => d.g === "no"), d => d.r), sB = d3.sum(datos.filter(d => d.g === "sí"), d => d.r);
      rd.innerHTML = modo ? `<div>Suma de rangos <b style="color:${COL.azul}">sin estancia ${f(sA, 1)}</b></div><div>Suma de rangos <b style="color:${COL.rojo}">con estancia ${f(sB, 1)}</b></div>
        <div>W = ${f(sA - 8 * 9 / 2, 1)}</div><div class="peq">Si no hubiera diferencias, cada grupo sumaría unos ${f(16 * 17 / 4, 0)}.</div>`
        : "Datos de recuento con un valor extremo (12): la media se distorsiona. Wilcoxon solo mira el <b>orden</b>.";
    }
    botones(panel, [{ etq: "Ordenar y asignar rangos", click: () => { modo = 1 - modo; dibuja(); } }]);
    panel.appendChild(rd); dibuja();
    return { enter() { modo = 0; dibuja(); } };
  };

  W.correlacion = el => {
    const { vis, panel } = marco(el), w = 760, h = 470, svg = lienzo(vis, w, h), st = { r: 0.5, atip: false };
    const z1 = d3.range(150).map(rnormGen(rng(31))), z2 = d3.range(150).map(rnormGen(rng(32)));
    const x = d3.scaleLinear([-3.5, 4], [60, 740]), y = d3.scaleLinear([-4, 4], [420, 20]);
    ejes(svg.append("g"), x, y, { y0: 420, x0: 60, xlab: "Variable X", ylab: "Variable Y", fx: () => "", fy: () => "" });
    const g = svg.append("g"), at = svg.append("circle").attr("r", 10).attr("fill", COL.rojo).attr("cx", x(3.6)).attr("cy", y(-3.6)).attr("opacity", 0);
    const ln = svg.append("line").attr("stroke", COL.rojo).attr("stroke-width", 3); const rd = lectura(panel, "grande");
    function dibuja() {
      const r = st.r, ys = z1.map((a, i) => r * a + Math.sqrt(1 - r * r) * z2[i]);
      g.selectAll("circle").data(z1).join("circle").attr("r", 5).attr("fill", COL.azul).attr("opacity", .55).attr("cx", v => x(v)).transition().duration(400).attr("cy", (v, i) => y(ys[i]));
      const X = z1.concat(st.atip ? [3.6] : []), Y = ys.concat(st.atip ? [-3.6] : []), rp = pearson(X, Y), rs = pearson(rangos(X), rangos(Y));
      const b = rp * de(Y) / de(X), a = media(Y) - b * media(X);
      ln.transition().duration(400).attr("x1", x(-3.5)).attr("x2", x(4)).attr("y1", y(a + b * -3.5)).attr("y2", y(a + b * 4));
      at.transition().duration(300).attr("opacity", st.atip ? 1 : 0);
      rd.innerHTML = `<div>Pearson r = <b>${f(rp, 2)}</b></div><div>Spearman ρ = <b>${f(rs, 2)}</b></div><div>r² = <b>${pct(rp * rp)}</b> de varianza compartida</div>` +
        (st.atip ? `<div class="peq rojo">Un solo punto extremo arrastra a Pearson; Spearman resiste mejor.</div>` : "");
    }
    deslizador(panel, { etq: "Correlación en la población", min: -1, max: 1, paso: 0.05, valor: 0.5, fmt: v => f(v, 2), cambio: v => { st.r = v; dibuja(); } });
    const lb = crear(crear(panel, "w-ctrl"), "w-chk", "label"), i = crear(lb, "", "input"); i.type = "checkbox"; lb.append(" Añadir un valor atípico");
    i.onchange = () => { st.atip = i.checked; dibuja(); };
    panel.appendChild(rd); dibuja();
  };

  /* =====================================================================
     SESIÓN 4 · CHI CUADRADO Y ANOVA
     ===================================================================== */

  W["chi-mosaico"] = el => {
    const X = CUR().chi, { vis, panel } = marco(el, { ancho: 470 }), w = 760, h = 470, svg = lienzo(vis, w, h);
    const O = X.O, tot = d3.sum(O.flat()), fil = O.map(r => d3.sum(r)), col = [0, 1].map(j => O[0][j] + O[1][j]);
    const E = O.map((r, i) => r.map((v, j) => fil[i] * col[j] / tot));
    const x0 = 90, ancho = 640, alto = 380, y0 = 30, colC = { correcto: COL.verde, incorrecto: COL.rojo };
    const celdas = []; O.forEach((r, i) => r.forEach((v, j) => celdas.push({ i, j, o: v, e: E[i][j], res: X.stdres[i][j] })));
    const g = svg.append("g").selectAll("g").data(celdas).join(en => { const gg = en.append("g"); gg.append("rect"); gg.append("text").attr("class", "celda grande").attr("text-anchor", "middle"); return gg; });
    svg.append("g").selectAll("text").data(X.filas).join("text").attr("class", "w-tag fuerte").attr("text-anchor", "middle")
      .attr("x", (d, i) => x0 + (i ? fil[0] / tot * ancho : 0) + fil[i] / tot * ancho / 2).attr("y", 440).text(d => "estancia: " + d);
    const ley = svg.append("g").attr("display", "none"); X.cols.forEach((c, j) => { ley.append("rect").attr("x", 10).attr("y", 60 + j * 30).attr("width", 16).attr("height", 16).attr("fill", colC[c]); ley.append("text").attr("x", 10).attr("y", 50 + j * 30 + 40).attr("class", "w-tag").text(""); });
    const rd = lectura(panel), tabla = crear(panel, "w-tabla"); let modo = "o";
    const div = d3.scaleDiverging([-4, 0, 4], t => d3.interpolateRgbBasis([COL.rojo, "#f6f6f6", COL.celeste])(t));
    function dibuja() {
      g.transition().duration(900).attr("transform", c => { const xx = x0 + (c.i ? fil[0] / tot * ancho : 0); const v = modo === "e" ? E[c.i] : O[c.i];
        const yy = y0 + (c.j ? v[0] / fil[c.i] * alto : 0); return `translate(${xx},${yy})`; });
      g.select("rect").transition().duration(900).attr("width", c => fil[c.i] / tot * ancho - 4).attr("height", c => (modo === "e" ? E : O)[c.i][c.j] / fil[c.i] * alto - 4)
        .attr("fill", c => modo === "r" ? div(c.res) : colC[X.cols[c.j]]).attr("rx", 6);
      g.select("text").transition().duration(900).attr("x", c => (fil[c.i] / tot * ancho) / 2).attr("y", c => (modo === "e" ? E : O)[c.i][c.j] / fil[c.i] * alto / 2 + 4)
        .text(c => modo === "o" ? `${c.o} ${X.cols[c.j]}` : modo === "e" ? `${f(c.e, 1)} esperados` : `residuo ${f(c.res, 2)}`).attr("fill", c => modo === "r" && Math.abs(c.res) < 2 ? COL.tinta : "#fff");
      rd.innerHTML = { o: "<b>Observadas.</b> Con estancia, el subjuntivo correcto es más frecuente.", e: "<b>Esperadas si no hubiera relación</b>: las dos columnas tendrían el mismo reparto.", r: "<b>Residuos estandarizados</b>: |r| > 1,96 → la celda se aleja de lo esperado." }[modo];
      const suma = d3.sum(celdas, c => (c.o - c.e) ** 2 / c.e);
      tabla.innerHTML = `<table><tr><th>estancia · subjuntivo</th><th>O</th><th>E</th><th>(O−E)²/E</th><th>residuo</th></tr>` + celdas.map(c => `<tr><td>${X.filas[c.i]} · ${X.cols[c.j]}</td><td>${c.o}</td><td>${f(c.e, 1)}</td><td>${f((c.o - c.e) ** 2 / c.e, 2)}</td><td>${f(c.res, 2)}</td></tr>`).join("") +
        `<tr><td colspan="3">Suma = χ² sin corrección</td><td><b>${f(suma, 2)}</b></td><td></td></tr><tr><td colspan="3">χ² de <code>chisq.test()</code> (con corrección de Yates)</td><td><b>${f(X.x2, 2)}</b></td><td></td></tr></table>
        <div class="peq">p ${fp(X.p)} · (O−E)²/E = contribución de la celda al χ²; residuo = residuo estandarizado, el que se dibuja (|r| > 1,96 → la celda se aparta de lo esperado). Yates resta 0,5 a cada |O − E| en las tablas 2 × 2.</div>`;
    }
    segmentado(panel, "Mostrar", [["o", "Observadas"], ["e", "Esperadas"], ["r", "Residuos"]], "o", v => { modo = v; dibuja(); });
    panel.appendChild(rd); panel.appendChild(tabla); dibuja();
  };

  W.fwer = el => {
    const { vis, panel } = marco(el), w = 800, h = 400, svg = lienzo(vis, w, h), st = { k: 6, bonf: false };
    const x = d3.scaleLinear([1, 30], [70, 770]), y = d3.scaleLinear([0, 1], [350, 20]);
    ejes(svg.append("g"), x, y, { y0: 350, xlab: "Número de comparaciones", ylab: "P(al menos un falso positivo)", fy: d3.format(".0%") });
    svg.append("line").attr("x1", 70).attr("x2", 770).attr("y1", y(0.05)).attr("y2", y(0.05)).attr("stroke", COL.tinta).attr("stroke-dasharray", "5 4");
    const curva = svg.append("path").attr("fill", "none").attr("stroke-width", 4), pto = svg.append("circle").attr("r", 10).attr("stroke", "#fff").attr("stroke-width", 3);
    const rd = lectura(panel, "grande");
    function dibuja() {
      const fw = k => 1 - Math.pow(1 - (st.bonf ? 0.05 / k : 0.05), k);
      curva.transition().duration(400).attr("d", d3.line().x(k => x(k)).y(k => y(fw(k)))(d3.range(1, 31))).attr("stroke", st.bonf ? COL.verde : COL.rojo);
      pto.transition().duration(300).attr("cx", x(st.k)).attr("cy", y(fw(st.k))).attr("fill", st.bonf ? COL.verde : COL.rojo);
      const g = Math.round((1 + Math.sqrt(1 + 8 * st.k)) / 2);
      rd.innerHTML = `<div><b>${st.k}</b> pruebas → riesgo de falsa alarma <b class="${st.bonf ? "verde" : "rojo"}">${pct(fw(st.k), 1)}</b></div>` +
        (g * (g - 1) / 2 === st.k ? `<div class="peq">Equivale a comparar ${g} grupos dos a dos.</div>` : "") +
        (st.bonf ? `<div class="peq">Bonferroni: cada prueba con α = 0,05 / ${st.k} = ${f(0.05 / st.k, 4)}</div>` : "");
    }
    deslizador(panel, { etq: "Comparaciones", min: 1, max: 30, valor: 6, cambio: v => { st.k = v; dibuja(); } });
    const lb = crear(crear(panel, "w-ctrl"), "w-chk", "label"), i = crear(lb, "", "input"); i.type = "checkbox"; lb.append(" Aplicar corrección de Bonferroni");
    i.onchange = () => { st.bonf = i.checked; dibuja(); };
    panel.appendChild(rd); dibuja();
  };

  W["anova-f"] = el => {
    const { vis, panel } = marco(el), w = 860, h = 460, svg = lienzo(vis, w, h), st = { sep: 4, s: 10 }, n = 25;
    const zs = L1S.map((l, k) => d3.range(n).map(rnormGen(rng(40 + k)))), js = L1S.map((l, k) => d3.range(n).map(rng(50 + k)));
    const x = d3.scaleBand(L1S, [80, 640]).padding(0.25), y = d3.scaleLinear([20, 120], [410, 20]);
    ejes(svg.append("g"), x, y, { y0: 410, ylab: "Nota" });
    const gm = svg.append("line").attr("x1", 80).attr("x2", 640).attr("stroke", COL.tinta).attr("stroke-dasharray", "6 4").attr("stroke-width", 2);
    const gP = svg.append("g"), gM = svg.append("g");
    const bar = svg.append("g").attr("transform", "translate(690,30)");
    bar.append("text").attr("class", "w-tag fuerte").attr("x", 0).attr("y", 0).text("Varianza media");
    const bE = bar.append("rect").attr("x", 0).attr("width", 60).attr("fill", COL.rojo), bD = bar.append("rect").attr("x", 80).attr("width", 60).attr("fill", COL.gris);
    bar.append("text").attr("class", "w-tag").attr("x", 30).attr("y", 395).attr("text-anchor", "middle").text("entre"); bar.append("text").attr("class", "w-tag").attr("x", 110).attr("y", 395).attr("text-anchor", "middle").text("dentro");
    const vE = bar.append("text").attr("class", "w-tag fuerte").attr("x", 30).attr("text-anchor", "middle"), vD = bar.append("text").attr("class", "w-tag fuerte").attr("x", 110).attr("text-anchor", "middle");
    svg.append("text").attr("class", "w-tag").attr("x", 80).attr("y", 450).text("Simulación: nota de 4 grupos de 25 estudiantes · rayas negras: media de cada grupo · discontinua: media general");
    const rd = lectura(panel, "grande"); const off = [-1.5, 0.5, 1.5, -0.5];
    function dibuja() {
      const grupos = L1S.map((l, k) => zs[k].map(z => 68 + off[k] * st.sep + z * st.s)), todos = grupos.flat(), gmed = media(todos);
      const ms = grupos.map(media), sse = d3.sum(grupos, (g, k) => d3.sum(g, v => (v - ms[k]) ** 2)), ssb = d3.sum(ms, m => n * (m - gmed) ** 2);
      const msb = ssb / 3, msw = sse / (4 * n - 4), F = msb / msw, p = pf(F, 3, 4 * n - 4), eta = ssb / (ssb + sse);
      gP.selectAll("g").data(grupos).join("g").each(function (g, k) {
        d3.select(this).selectAll("circle").data(g).join("circle").attr("r", 4.5).attr("fill", COL_L1[L1S[k]]).attr("opacity", .55)
          .attr("cx", (v, i) => x(L1S[k]) + x.bandwidth() / 2 + (js[k][i] - .5) * x.bandwidth() * .7).transition().duration(300).attr("cy", v => y(Math.max(20, Math.min(120, v))));
      });
      gM.selectAll("line").data(ms).join("line").attr("x1", (m, k) => x(L1S[k])).attr("x2", (m, k) => x(L1S[k]) + x.bandwidth()).attr("stroke", COL.tinta).attr("stroke-width", 5).transition().duration(300).attr("y1", m => y(m)).attr("y2", m => y(m));
      gm.transition().duration(300).attr("y1", y(gmed)).attr("y2", y(gmed));
      const yb = d3.scaleLinear([0, Math.max(msb, msw, 400)], [370, 20]);
      bE.transition().duration(300).attr("y", yb(msb)).attr("height", 370 - yb(msb)); bD.transition().duration(300).attr("y", yb(msw)).attr("height", 370 - yb(msw));
      vE.attr("y", yb(msb) - 6).text(f(msb, 0)); vD.attr("y", yb(msw) - 6).text(f(msw, 0));
      rd.innerHTML = `<div>F = entre / dentro = ${f(msb, 0)} / ${f(msw, 0)} = <b>${f(F, 2)}</b></div><div>p ${fp(p)}</div>
        <div>η² = SC entre / SC total = ${f(ssb, 0)} / ${f(ssb + sse, 0)} = <b>${f(eta, 3)}</b></div>
        <div class="peq">El ${pct(eta)} de la variación de las notas se debe a la diferencia entre grupos; el ${pct(1 - eta)}, a diferencias entre personas del mismo grupo. Las barras son varianzas medias (puntos²): la gris ≈ DE² dentro de los grupos.</div>`;
    }
    deslizador(panel, { etq: "Separación entre las medias", min: 0, max: 10, paso: 0.5, valor: 4, fmt: v => f(v, 1), cambio: v => { st.sep = v; dibuja(); } });
    deslizador(panel, { etq: "Variabilidad dentro de cada grupo", min: 2, max: 20, paso: 0.5, valor: 10, fmt: v => f(v, 1), cambio: v => { st.s = v; dibuja(); } });
    panel.appendChild(rd); dibuja();
  };

  W.tukey = el => {
    const Tk = CUR().tukey, { vis, panel } = marco(el), w = 860, h = 420, svg = lienzo(vis, w, h);
    const y = d3.scaleBand(Tk.map(d => d.comp), [20, 370]).padding(0.35), x = d3.scaleLinear([d3.min(Tk, d => d.lwr) - 1, d3.max(Tk, d => d.upr) + 1], [190, 840]);
    ejes(svg.append("g"), x, null, { y0: 375, sinY: true, xlab: "Diferencia de medias (IC 95 %)" });
    svg.append("line").attr("x1", x(0)).attr("x2", x(0)).attr("y1", 10).attr("y2", 375).attr("stroke", COL.tinta).attr("stroke-dasharray", "6 4");
    const filas = svg.append("g").selectAll("g").data(Tk).join("g").attr("transform", d => `translate(0,${y(d.comp) + y.bandwidth() / 2})`).style("cursor", "pointer");
    filas.append("rect").attr("x", 0).attr("width", 850).attr("y", -y.step() / 2).attr("height", y.step()).attr("fill", "transparent");
    filas.append("text").attr("x", 180).attr("y", 5).attr("text-anchor", "end").attr("class", "w-tag fuerte").text(d => d.comp);
    const sig = d => d.p < 0.05;
    filas.append("line").attr("x1", d => x(d.lwr)).attr("x2", d => x(d.lwr)).attr("stroke", d => sig(d) ? COL.rojo : COL.gris).attr("stroke-width", 5)
      .transition().delay((d, i) => i * 150).duration(600).attr("x2", d => x(d.upr));
    filas.append("circle").attr("cx", d => x(d.diff)).attr("r", 8).attr("fill", d => sig(d) ? COL.rojo : COL.gris);
    const rd = lectura(panel, "grande");
    const frase = d => { const [a, b] = d.comp.split("-"), mas = d.diff > 0 ? a : b, menos = d.diff > 0 ? b : a;
      return `Los estudiantes de L1 <b>${mas}</b> sacan de media <b>${f(Math.abs(d.diff), 1)} puntos más</b> que los de L1 <b>${menos}</b>.<br>`; };
    const msg = d => `<b>${d.comp.replace("-", " − ")}</b><br>${frase(d)}Diferencia ${signo(d.diff, 1)} puntos<br>IC 95 % [${f(d.lwr, 1)}, ${f(d.upr, 1)}]<br>p ajustado ${fp(d.p)}<br>` +
      (sig(d) ? `<span class="rojo">El intervalo no cruza el 0: diferencia significativa.</span>` : `<span class="peq">Cruza el 0: no podemos afirmar que difieran.</span>`);
    filas.on("mouseenter click", (e, d) => { rd.innerHTML = msg(d); filas.attr("opacity", o => o === d ? 1 : 0.4); });
    rd.innerHTML = "Variable: <b>nota DELE</b> según la L1. Pasa el ratón por cada comparación.";
  };

  W.interaccion = el => {
    const I = CUR().interaccion, { vis, panel } = marco(el), w = 760, h = 420, svg = lienzo(vis, w, h);
    const x = d3.scalePoint(["no", "sí"], [120, 620]).padding(0.2), y = d3.scaleLinear([45, 90], [370, 20]);
    ejes(svg.append("g"), x, y, { y0: 370, xlab: "Estancia", ylab: "Nota media" });
    const esc = {
      real: L1S.map(l => [I.find(d => d.L1 === l && d.estancia === "no").m, I.find(d => d.L1 === l && d.estancia === "sí").m]),
      sin: [[60, 66], [55, 61], [68, 74], [72, 78]], ord: [[60, 63], [55, 57], [66, 78], [70, 86]], cruz: [[70, 58], [55, 68], [66, 78], [78, 62]] };
    const txt = { real: "<b>Nuestros datos</b>: líneas casi paralelas → la estancia suma más o menos lo mismo en todas las L1 (interacción no significativa).",
      sin: "<b>Sin interacción</b> (escenario ficticio, datos inventados): líneas paralelas. Cada factor suma por su cuenta.", ord: "<b>Interacción ordinal</b> (escenario ficticio, datos inventados): la estancia ayuda a todos, pero mucho más a unos que a otros.",
      cruz: "<b>Interacción cruzada</b> (escenario ficticio, datos inventados): la estancia ayuda a unos y perjudica a otros. ¡Los efectos principales engañan!" };
    const marca = svg.append("text").attr("class", "w-tag fuerte").attr("x", 740).attr("y", 30).attr("text-anchor", "end").attr("fill", COL.rojo);
    const g = svg.append("g").selectAll("g").data(L1S).join(en => { const gg = en.append("g"); gg.append("line"); gg.append("circle").attr("class", "a"); gg.append("circle").attr("class", "b"); gg.append("text"); return gg; });
    g.select("line").attr("stroke", l => COL_L1[l]).attr("stroke-width", 4); g.selectAll("circle").attr("r", 8).attr("fill", l => COL_L1[l]);
    g.select("text").attr("class", "w-tag fuerte").attr("fill", l => COL_L1[l]).attr("x", x("sí") + 14).text(l => l);
    const rd = lectura(panel, "grande");
    function dibuja(k) {
      const v = esc[k];
      g.each(function (l, i) { const s = d3.select(this).transition().duration(900);
        s.select("line").attr("x1", x("no")).attr("x2", x("sí")).attr("y1", y(v[i][0])).attr("y2", y(v[i][1]));
        s.select(".a").attr("cx", x("no")).attr("cy", y(v[i][0])); s.select(".b").attr("cx", x("sí")).attr("cy", y(v[i][1])); s.select("text").attr("y", y(v[i][1]) + 5); });
      rd.innerHTML = txt[k];
      marca.text(k === "real" ? "" : "ESCENARIO FICTICIO");
    }
    segmentado(panel, "Escenario", [["real", "Nuestros datos (reales)"], ["sin", "Sin interacción (ficticio)"], ["ord", "Ordinal (ficticio)"], ["cruz", "Cruzada (ficticio)"]], "real", dibuja);
    panel.appendChild(rd); dibuja("real");
  };

  /* =====================================================================
     SESIÓN 5 · CORPUS Y MODELOS
     ===================================================================== */

  function frecCalc() {
    const F = CUR().frec, N1 = CUR().N.aprend, N2 = CUR().N.nativo;
    return F.map(d => { const a = d.aprend, b = d.nativo, E1 = N1 * (a + b) / (N1 + N2), E2 = N2 * (a + b) / (N1 + N2);
      return { ...d, p1: a / N1 * 1e6, p2: b / N2 * 1e6, LL: 2 * (a * Math.log(a / E1) + b * Math.log(b / E2)), LR: Math.log2((a / N1) / (b / N2)) }; });
  }

  W.keyness = el => {
    const F = frecCalc(), { vis, panel } = marco(el), w = 860, h = 470, svg = lienzo(vis, w, h), st = { ll: 15.13, lr: 1 };
    const x = d3.scaleLinear([-3, 1.5], [70, 830]), y = d3.scaleLinear([0, d3.max(F, d => d.LL) * 1.1], [420, 20]);
    const fondo = svg.append("g"); ejes(svg.append("g"), x, y, { y0: 420, x0: 70, xlab: "Log-ratio: tamaño del efecto (− infrauso · + sobreuso)", ylab: "Log-likelihood: fiabilidad" });
    const hl = svg.append("line").attr("x1", 70).attr("x2", 830).attr("stroke", COL.tinta).attr("stroke-dasharray", "6 4");
    const vl = svg.append("g").selectAll("line").data([-1, 1]).join("line").attr("y1", 20).attr("y2", 420).attr("stroke", COL.gris).attr("stroke-dasharray", "3 4");
    const pts = svg.append("g").selectAll("g").data(F).join(en => { const g = en.append("g").style("cursor", "pointer"); g.append("circle").attr("r", 11); g.append("text").attr("class", "w-tag fuerte").attr("y", -16).attr("text-anchor", "middle"); return g; });
    pts.attr("transform", d => `translate(${x(d.LR)},${y(d.LL)})`).select("text").text(d => d.palabra);
    const rd = lectura(panel), lista = crear(panel, "w-read");
    function dibuja() {
      hl.transition().attr("y1", y(st.ll)).attr("y2", y(st.ll)); vl.transition().attr("x1", v => x(v * st.lr)).attr("x2", v => x(v * st.lr));
      fondo.selectAll("rect").data([[x(-3), x(-st.lr)], [x(st.lr), x(1.5)]]).join("rect").attr("y", 20).attr("height", y(st.ll) - 20)
        .attr("fill", (d, i) => i ? COL.azul : COL.rojo).attr("opacity", .07).transition().attr("x", d => d[0]).attr("width", d => Math.max(0, d[1] - d[0]));
      const clave = d => d.LL >= st.ll && Math.abs(d.LR) >= st.lr;
      pts.select("circle").transition().attr("fill", d => clave(d) ? (d.LR > 0 ? COL.azul : COL.rojo) : COL.gris);
      const pos = F.filter(d => clave(d) && d.LR > 0), neg = F.filter(d => clave(d) && d.LR < 0);
      lista.innerHTML = `<div><b style="color:${COL.azul}">Sobreuso:</b> ${pos.map(d => d.palabra).join(", ") || "—"}</div><div><b style="color:${COL.rojo}">Infrauso:</b> ${neg.map(d => d.palabra).join(", ") || "—"}</div>`;
    }
    pts.on("mouseenter click", (e, d) => { rd.innerHTML = `<b>${d.palabra}</b>: ${d.aprend} (aprend.) vs ${d.nativo} (nativos)<br>${f(d.p1, 0)} vs ${f(d.p2, 0)} pmw<br>LL = <b>${f(d.LL, 1)}</b> · LR = <b>${f(d.LR, 2)}</b><br>` +
      (d.LR > 0 ? `Los estudiantes la usan ${f(2 ** d.LR, 1)} veces más.` : `Los estudiantes la usan ${f(2 ** -d.LR, 1)} veces menos.`); });
    segmentado(panel, "Umbral de LL", [["3.84", "p < 0,05"], ["6.63", "p < 0,01"], ["15.13", "p < 0,0001"]], "15.13", v => { st.ll = +v; dibuja(); });
    deslizador(panel, { etq: "|log-ratio| mínimo", min: 0, max: 2, paso: 0.1, valor: 1, fmt: v => f(v, 1) + ` (×${f(2 ** v, 1)})`, cambio: v => { st.lr = v; dibuja(); } });
    panel.appendChild(lista); panel.appendChild(rd); rd.innerHTML = "<span class='peq'>Pasa el ratón por una palabra.</span>"; dibuja();
  };

  W["simple-maths"] = el => {
    const F = frecCalc(), { vis, panel } = marco(el), w = 860, h = 470, svg = lienzo(vis, w, h);
    const y = d3.scaleBand(F.map(d => d.palabra), [20, 440]).padding(0.18), gx = svg.append("g");
    const filas = svg.append("g").selectAll("g").data(F, d => d.palabra).join(en => { const g = en.append("g"); g.append("rect"); g.append("text").attr("class", "w-tag fuerte nombre").attr("x", -8).attr("text-anchor", "end"); g.append("text").attr("class", "w-tag val"); return g; });
    filas.select(".nombre").text(d => d.palabra);
    const rd = lectura(panel, "grande");
    function dibuja(lg) {
      const n = Math.pow(10, lg); F.forEach(d => { d.sm = (d.p1 + n) / (d.p2 + n); });
      const ord = [...F].sort((a, b) => b.sm - a.sm); y.domain(ord.map(d => d.palabra));
      const x = d3.scaleLog([0.1, 3], [130, 820]);
      gx.selectAll("*").remove(); ejes(gx, x, null, { y0: 450, sinY: true, tx: 8, fx: v => f(v, 1), xlab: "Simple Maths (escala log; 1 = igual)" });
      gx.append("line").attr("x1", x(1)).attr("x2", x(1)).attr("y1", 10).attr("y2", 450).attr("stroke", COL.tinta).attr("stroke-dasharray", "5 4");
      filas.transition().duration(700).attr("transform", d => `translate(0,${y(d.palabra)})`);
      filas.select("rect").attr("height", y.bandwidth()).transition().duration(700).attr("x", d => Math.min(x(1), x(d.sm))).attr("width", d => Math.abs(x(d.sm) - x(1))).attr("fill", d => d.sm >= 1 ? COL.azul : COL.rojo);
      filas.select(".nombre").attr("x", 120).attr("y", y.bandwidth() / 2 + 6);
      filas.select(".val").transition().duration(700).attr("x", d => (d.sm >= 1 ? x(d.sm) + 6 : x(d.sm) - 6)).attr("text-anchor", d => d.sm >= 1 ? "start" : "end").attr("y", y.bandwidth() / 2 + 6).text(d => f(d.sm, 2));
      const t0 = ord[0], pu = F.find(d => d.palabra === "pues"), cal = d => `(${f(d.p1, 0)} + ${n < 1 ? f(n, 2) : f(n, 0)}) / (${f(d.p2, 0)} + ${n < 1 ? f(n, 2) : f(n, 0)}) = <b>${f(d.sm, 2)}</b>`;
      rd.innerHTML = `n = <b>${n < 1 ? f(n, 2) : f(n, 0)}</b> <span class="peq">(constante de suavizado, no un tamaño de muestra)</span> · más sobreusada: <b>${ord[0].palabra}</b>
        <div class="peq"><b>${t0.palabra}</b>: ${cal(t0)} · <b>pues</b>: ${cal(pu)} <br>(pmw estudiantes + n) / (pmw nativos + n). &gt; 1 sobreuso · &lt; 1 infrauso.</div><div class="peq">${lg < 1 ? "n bajo: pesan las palabras de frecuencia baja." : lg > 2.5 ? "n alto: todo se acerca a 1; destacan las palabras más frecuentes." : "Valor intermedio."}</div>`;
    }
    deslizador(panel, { etq: "Parámetro n (log₁₀)", min: -1, max: 4, paso: 0.1, valor: 0, fmt: v => f(Math.pow(10, v), v < 0 ? 1 : 0), cambio: dibuja });
    panel.appendChild(rd); dibuja(0);
  };

  W.regresion = el => {
    const O = CUR().ols, D = O.ids.map(i => CUR().ele[i - 1]), { vis, panel } = marco(el), w = 860, h = 460, svg = lienzo(vis, w, h);
    const st = { b0: 60, b1: 0.02, cuad: true }, x = d3.scaleLinear([0, 500], [70, 840]), y = d3.scaleLinear([20, 110], [410, 20]), u = (x(1) - x(0));
    ejes(svg.append("g"), x, y, { y0: 410, xlab: "Horas de instrucción", ylab: "Nota DELE" });
    const gQ = svg.append("g"), gR = svg.append("g"), ln = svg.append("line").attr("stroke", COL.azul).attr("stroke-width", 4);
    svg.append("g").selectAll("circle").data(D).join("circle").attr("cx", d => x(d.horas)).attr("cy", d => y(d.nota_dele)).attr("r", 6).attr("fill", COL.azul);
    const sst = d3.sum(D, d => (d.nota_dele - media(D.map(e => e.nota_dele))) ** 2), sseMin = d3.sum(D, d => (d.nota_dele - O.b0 - O.b1 * d.horas) ** 2);
    const rd = lectura(panel, "grande"); let s0, s1;
    function dibuja(dur = 200) {
      const p = h_ => st.b0 + st.b1 * h_;
      ln.transition().duration(dur).attr("x1", x(0)).attr("x2", x(500)).attr("y1", y(p(0))).attr("y2", y(p(500)));
      gR.selectAll("line").data(D).join("line").attr("stroke", COL.rojo).attr("stroke-width", 2).attr("x1", d => x(d.horas)).attr("x2", d => x(d.horas)).attr("y1", d => y(d.nota_dele)).transition().duration(dur).attr("y2", d => y(p(d.horas)));
      const ky = y(0) - y(1);
      gQ.selectAll("rect").data(st.cuad ? D : []).join("rect").attr("fill", COL.rojo).attr("opacity", 0.08).attr("stroke", COL.rojo).attr("stroke-opacity", .25)
        .transition().duration(dur).attr("x", d => x(d.horas)).attr("y", d => y(Math.max(d.nota_dele, p(d.horas)))).attr("width", d => Math.abs(d.nota_dele - p(d.horas)) * ky).attr("height", d => Math.abs(d.nota_dele - p(d.horas)) * ky);
      const sse = d3.sum(D, d => (d.nota_dele - p(d.horas)) ** 2);
      rd.innerHTML = `<div>nota = <b>${f(st.b0, 1)}</b> + <b>${f(st.b1, 3)}</b> × horas</div><div>Suma de cuadrados de los residuos <b class="rojo">${f(sse, 0)}</b></div>
        <div class="peq">Mínimo posible (lm): ${f(sseMin, 0)} · R² de tu recta: ${f(1 - sse / sst, 2)}</div>
        <div>Predicción para alguien con 300 horas: ${f(st.b0, 1)} + ${f(st.b1, 3)} × 300 = <b>${f(p(300), 1)}</b></div>` + (sse - sseMin < 30 ? `<div class="verde"><b>¡Has encontrado la recta de mínimos cuadrados!</b></div>` : "");
    }
    s0 = deslizador(panel, { etq: "Intercepto b₀", min: 20, max: 90, paso: 0.5, valor: 60, fmt: v => f(v, 1), cambio: v => { st.b0 = v; dibuja(); } });
    s1 = deslizador(panel, { etq: "Pendiente b₁", min: -0.05, max: 0.15, paso: 0.001, valor: 0.02, fmt: v => f(v, 3), cambio: v => { st.b1 = v; dibuja(); } });
    const lb = crear(crear(panel, "w-ctrl"), "w-chk", "label"), i = crear(lb, "", "input"); i.type = "checkbox"; i.checked = true; lb.append(" Dibujar los cuadrados de los residuos");
    i.onchange = () => { st.cuad = i.checked; dibuja(); };
    botones(panel, [{ etq: "Ajuste de lm()", click: () => { st.b0 = O.b0; st.b1 = O.b1; s0.set(O.b0, true); s1.set(O.b1, true); dibuja(1200); } }]);
    panel.appendChild(rd); dibuja();
  };

  W.logistica = el => {
    const D = CUR().ele, G = CUR().glm1, { vis, panel } = marco(el), w = 860, h = 440, svg = lienzo(vis, w, h);
    const st = { b0: -2, b1: 0.008, hh: 250 }, x = d3.scaleLinear([0, 520], [70, 840]), y = d3.scaleLinear([-0.1, 1.1], [400, 20]); const r = rng(9);
    ejes(svg.append("g"), x, y, { y0: 400, xlab: "Horas de instrucción", ylab: "P(subjuntivo correcto)", fy: v => f(v, 1) });
    svg.append("g").selectAll("circle").data(D).join("circle").attr("cx", d => x(d.horas)).attr("cy", d => y((d.subjuntivo === "correcto" ? 1 : 0) + (r() - 0.5) * 0.12)).attr("r", 4).attr("fill", d => d.subjuntivo === "correcto" ? COL.verde : COL.rojo).attr("opacity", .4);
    const tramos = d3.range(0, 550, 50).map(a => { const s = D.filter(d => d.horas > a && d.horas <= a + 50); return { x: a + 25, n: s.length, p: s.filter(d => d.subjuntivo === "correcto").length / s.length }; }).filter(t => t.n >= 5);
    svg.append("g").selectAll("path").data(tramos).join("path").attr("d", d3.symbol(d3.symbolDiamond, 180)).attr("transform", t => `translate(${x(t.x)},${y(t.p)})`).attr("fill", COL.arena).attr("stroke", "#fff").attr("stroke-width", 1.5)
      .append("title").text(t => `${t.x - 25}–${t.x + 25} h: ${pct(t.p)} aciertan (n = ${t.n})`);
    svg.append("text").attr("class", "w-tag").attr("x", 80).attr("y", 14).text("◆ proporción real de aciertos en cada tramo de 50 horas · arriba (1): aciertan · abajo (0): fallan");
    const curva = svg.append("path").attr("fill", "none").attr("stroke", COL.azul).attr("stroke-width", 4);
    const sonda = svg.append("g"); sonda.append("line").attr("class", "v").attr("stroke", COL.tinta).attr("stroke-dasharray", "4 4"); sonda.append("circle").attr("r", 9).attr("fill", COL.arena).attr("stroke", "#fff").attr("stroke-width", 3);
    const rd = lectura(panel, "grande"); let s0, s1;
    function dibuja(dur = 150) {
      const p = hh => 1 / (1 + Math.exp(-(st.b0 + st.b1 * hh)));
      curva.transition().duration(dur).attr("d", d3.line().x(v => x(v)).y(v => y(p(v)))(d3.range(0, 521, 4)));
      sonda.select(".v").attr("x1", x(st.hh)).attr("x2", x(st.hh)).attr("y1", y(0)).attr("y2", y(1));
      sonda.select("circle").transition().duration(dur).attr("cx", x(st.hh)).attr("cy", y(p(st.hh)));
      const lo = st.b0 + st.b1 * st.hh, pp = p(st.hh);
      rd.innerHTML = `<div>log-odds = ${f(st.b0, 2)} + ${f(st.b1, 4)} × ${st.hh} = <b>${f(lo, 2)}</b></div><div>odds = e<sup>${f(lo, 2)}</sup> = <b>${f(Math.exp(lo), 2)}</b></div>
        <div>probabilidad = odds / (1 + odds) = <b class="rojo">${pct(pp)}</b></div><div class="peq">Es decir, de cada 100 estudiantes con ${st.hh} horas, unos ${f(100 * pp, 0)} aciertan y ${f(100 - 100 * pp, 0)} fallan: ${f(Math.exp(lo), 2)} aciertos por cada fallo.</div><div class="peq">Odds ratio por cada 100 horas más: ×${f(Math.exp(st.b1 * 100), 2)}</div>`;
    }
    s0 = deslizador(panel, { etq: "Intercepto b₀", min: -8, max: 2, paso: 0.05, valor: -2, fmt: v => f(v, 2), cambio: v => { st.b0 = v; dibuja(); } });
    s1 = deslizador(panel, { etq: "Pendiente b₁", min: 0, max: 0.03, paso: 0.0005, valor: 0.008, fmt: v => f(v, 4), cambio: v => { st.b1 = v; dibuja(); } });
    deslizador(panel, { etq: "Consultar: horas", min: 0, max: 500, paso: 10, valor: 250, cambio: v => { st.hh = v; dibuja(); } });
    botones(panel, [{ etq: "Ajuste de glm()", click: () => { st.b0 = G.b0; st.b1 = G.b1; s0.set(G.b0, true); s1.set(G.b1, true); dibuja(1200); } }]);
    panel.appendChild(rd); dibuja();
  };

  /* =====================================================================
     ÁRBOLES DE DECISIÓN
     ===================================================================== */

  W["anatomia-arbol"] = el => {
    const { vis, panel } = marco(el), w = 860, h = 460, svg = lienzo(vis, w, h);
    const N = [
      { id: "raiz", x: 430, y: 60, t: ["¿horas ≥ 243,5?"], c: COL.azul, exp: "<b>Nodo raíz</b>: contiene a los 240 estudiantes. Su pregunta es la que <b>mejor</b> separa las clases de todos los datos." },
      { id: "int", x: 250, y: 210, t: ["¿nivel = C1?"], c: COL.celeste, exp: "<b>Nodo interno</b>: una nueva pregunta, solo para los estudiantes que han llegado hasta aquí (161)." },
      { id: "h1", x: 680, y: 210, t: ["incorrecto", "81 % · n = 79"], c: COL.rojo, hoja: 1, exp: "<b>Hoja</b> (nodo terminal): ya no se divide. Predice la clase mayoritaria: <b>incorrecto</b> (81 % de los 79 casos)." },
      { id: "h2", x: 110, y: 370, t: ["correcto", "82 % · n = 62"], c: COL.verde, hoja: 1, exp: "<b>Hoja</b>: estudiantes con muchas horas y nivel C1. El 82 % usa bien el subjuntivo." },
      { id: "h3", x: 390, y: 370, t: ["correcto", "59 % · n = 99"], c: "#7fbfb3", hoja: 1, exp: "<b>Hoja impura</b>: predice «correcto», pero con solo un 59 %. Aquí el árbol duda: es un buen candidato para seguir dividiendo… o no." }];
    const E = [{ a: "raiz", b: "int", t: "sí" }, { a: "raiz", b: "h1", t: "no" }, { a: "int", b: "h2", t: "sí" }, { a: "int", b: "h3", t: "no" }];
    const by = Object.fromEntries(N.map(n => [n.id, n]));
    const ge = svg.append("g").selectAll("g").data(E).join("g").style("cursor", "pointer");
    ge.append("path").attr("d", e => `M${by[e.a].x},${by[e.a].y + 30}C${by[e.a].x},${(by[e.a].y + by[e.b].y) / 2} ${by[e.b].x},${(by[e.a].y + by[e.b].y) / 2} ${by[e.b].x},${by[e.b].y - 30}`)
      .attr("fill", "none").attr("stroke", COL.gris).attr("stroke-width", 4);
    ge.append("text").attr("class", "w-tag fuerte").attr("x", e => (by[e.a].x + by[e.b].x) / 2 + (by[e.b].x < by[e.a].x ? -16 : 16)).attr("y", e => (by[e.a].y + by[e.b].y) / 2 + 4)
      .attr("text-anchor", "middle").text(e => e.t);
    const gn = svg.append("g").selectAll("g").data(N).join("g").attr("transform", n => `translate(${n.x},${n.y})`).style("cursor", "pointer");
    gn.append("rect").attr("x", -100).attr("y", -32).attr("width", 200).attr("height", 64).attr("rx", n => n.hoja ? 12 : 32).attr("fill", n => n.c);
    gn.append("text").attr("class", "celda").attr("text-anchor", "middle").selectAll("tspan").data(n => n.t).join("tspan").attr("x", 0).attr("dy", (t, i, a) => i ? 22 : (a.length > 1 ? -4 : 7)).text(t => t);
    const etiquetas = [["Raíz", 430, 18], ["Rama", 230, 128], ["Nodo interno", 75, 175], ["Hojas", 250, 438]];
    svg.append("g").selectAll("text").data(etiquetas).join("text").attr("class", "w-tag anot").attr("x", d => d[1]).attr("y", d => d[2]).attr("text-anchor", "middle").text(d => d[0]);
    const rd = lectura(panel, "grande");
    gn.on("mouseenter click", (e, n) => { rd.innerHTML = n.exp; gn.attr("opacity", m => m === n ? 1 : 0.45); });
    ge.on("mouseenter click", (e, d) => { rd.innerHTML = `<b>Rama</b>: la respuesta a la pregunta del nodo. Por convención, en <code>rpart.plot</code> la rama <b>izquierda</b> es el «sí».`; });
    rd.innerHTML = "Pasa el ratón por cada parte del árbol.";
  };

  W.gini = el => {
    const { vis, panel } = marco(el), w = 860, h = 420, svg = lienzo(vis, w, h), st = { k: 14, ent: false };
    const bolas = svg.append("g").attr("transform", "translate(40,60)");
    svg.append("rect").attr("x", 25).attr("y", 40).attr("width", 290).attr("height", 250).attr("rx", 18).attr("fill", "#f5f7fa").attr("stroke", COL.claro).lower();
    svg.append("text").attr("class", "w-tag fuerte").attr("x", 170).attr("y", 28).attr("text-anchor", "middle").text("Un nodo con 20 estudiantes");
    const x = d3.scaleLinear([0, 1], [400, 840]), y = d3.scaleLinear([0, 1], [360, 40]);
    ejes(svg.append("g"), x, y, { y0: 360, x0: 400, xlab: "Proporción de «correcto» en el nodo", ylab: "Impureza", fx: d3.format(".0%"), fy: v => f(v, 1) });
    const cG = svg.append("path").attr("fill", "none").attr("stroke", COL.rojo).attr("stroke-width", 4), cE = svg.append("path").attr("fill", "none").attr("stroke", COL.celeste).attr("stroke-width", 3).attr("stroke-dasharray", "7 5");
    const ps = d3.range(0, 1.001, 0.01); cG.attr("d", d3.line().x(p => x(p)).y(p => y(1 - p * p - (1 - p) ** 2))(ps));
    const pG = svg.append("circle").attr("r", 10).attr("fill", COL.rojo).attr("stroke", "#fff").attr("stroke-width", 3), pE = svg.append("circle").attr("r", 8).attr("fill", COL.celeste);
    const rd = lectura(panel, "grande");
    function dibuja() {
      const p = st.k / 20, g = 1 - p * p - (1 - p) ** 2, ent = p === 0 || p === 1 ? 0 : -(p * Math.log2(p) + (1 - p) * Math.log2(1 - p));
      bolas.selectAll("circle").data(d3.range(20)).join("circle").attr("cx", i => 30 + (i % 5) * 52).attr("cy", i => 30 + Math.floor(i / 5) * 52).attr("r", 20)
        .transition().duration(300).attr("fill", i => i < st.k ? COL.verde : COL.rojo);
      pG.transition().duration(300).attr("cx", x(p)).attr("cy", y(g));
      cE.attr("d", st.ent ? d3.line().x(p => x(p)).y(p => y(p === 0 || p === 1 ? 0 : -(p * Math.log2(p) + (1 - p) * Math.log2(1 - p))))(ps) : null);
      pE.attr("opacity", st.ent ? 1 : 0).transition().duration(300).attr("cx", x(p)).attr("cy", y(ent));
      rd.innerHTML = `<div>${st.k} correctos · ${20 - st.k} incorrectos</div><div>Gini = 1 − ${f(p, 2)}² − ${f(1 - p, 2)}² = <b class="rojo">${f(g, 3)}</b></div>` +
        (st.ent ? `<div>Entropía = <b style="color:${COL.celeste}">${f(ent, 3)}</b></div>` : "") +
        `<div class="peq">${g === 0 ? "<b>Nodo puro</b>: todos iguales. Impureza 0." : g >= 0.49 ? "<b>Máxima mezcla</b>: 50/50. El nodo no sabe qué predecir." : "Cuanto más mezclado, más impuro."}</div>`;
    }
    deslizador(panel, { etq: "Estudiantes con subjuntivo correcto", min: 0, max: 20, valor: 14, cambio: v => { st.k = v; dibuja(); } });
    const lb = crear(crear(panel, "w-ctrl"), "w-chk", "label"), i = crear(lb, "", "input"); i.type = "checkbox"; lb.append(" Comparar con la entropía");
    i.onchange = () => { st.ent = i.checked; dibuja(); };
    panel.appendChild(rd); dibuja();
  };

  W["mejor-corte"] = el => {
    const D = CUR().ele, { vis, panel } = marco(el, { ancho: 400 }), w = 900, h = 540, svg = lienzo(vis, w, h);
    const ok = d => d.subjuntivo === "correcto", nT = D.length, aT = D.filter(ok).length, gPadre = gini(aT, nT - aT);
    const hs = [...new Set(D.map(d => d.horas))].sort(d3.ascending), cortes = hs.slice(1).map((v, i) => (v + hs[i]) / 2);
    const evalCorte = c => { const L = D.filter(d => d.horas < c), R = D.filter(d => d.horas >= c), aL = L.filter(ok).length, aR = R.filter(ok).length;
      const gL = gini(aL, L.length - aL), gR = gini(aR, R.length - aR); return { c, L: L.length, R: R.length, aL, aR, gL, gR, gan: gPadre - (L.length * gL + R.length * gR) / nT }; };
    const curvaH = cortes.map(evalCorte), mejorH = curvaH.reduce((a, b) => b.gan > a.gan ? b : a);
    function mejorCat(v) {
      const niv = [...new Set(D.map(d => d[v]))].map(k => { const s = D.filter(d => d[v] === k); return { k, n: s.length, p: s.filter(ok).length / s.length }; }).sort((a, b) => b.p - a.p);
      const ops = d3.range(1, niv.length).map(i => { const izq = niv.slice(0, i).map(d => d.k), L = D.filter(d => izq.includes(d[v])), R = D.filter(d => !izq.includes(d[v]));
        const aL = L.filter(ok).length, aR = R.filter(ok).length, gan = gPadre - (L.length * gini(aL, L.length - aL) + R.length * gini(aR, R.length - aR)) / nT;
        return { i, izq, der: niv.slice(i).map(d => d.k), gan, L: L.length, R: R.length, aL, aR }; });
      return { niv, ops, mejor: ops.reduce((a, b) => b.gan > a.gan ? b : a) };
    }
    const cat = { nivel: mejorCat("nivel"), L1: mejorCat("L1"), estancia: mejorCat("estancia") };
    const gan = { horas: mejorH.gan, nivel: cat.nivel.mejor.gan, L1: cat.L1.mejor.gan, estancia: cat.estancia.mejor.gan };
    const top = svg.append("g"), bot = svg.append("g").attr("transform", "translate(0,330)");
    const rd = lectura(panel, "grande"); const caja = crear(panel, "w-hijos");
    let modo = "horas", desl;
    function hijos(r, etL, etR) {
      const b = (n, a, et) => `<div class="hijo"><div class="et">${et}</div><div class="barra"><span style="width:${100 * a / n}%"></span></div><div>n = ${n} · ${pct(a / n)} correcto</div><div>Gini <b>${f(gini(a, n - a), 3)}</b></div></div>`;
      caja.innerHTML = b(r.L, r.aL, etL) + b(r.R, r.aR, etR);
    }
    const x = d3.scaleLinear(d3.extent(D, d => d.horas), [60, 860]), r = rng(13);
    D.forEach(d => { d._j = r(); });
    function dibujaHoras(c) {
      const ev = evalCorte(c);
      top.selectAll(".corte").data([c]).join("line").attr("class", "corte").attr("y1", 22).attr("y2", 270).attr("stroke", COL.tinta).attr("stroke-width", 3).attr("x1", x(c)).attr("x2", x(c));
      top.selectAll(".ladoL").data([c]).join("rect").attr("class", "ladoL").attr("x", 60).attr("y", 10).attr("height", 250).attr("fill", COL.celeste).attr("opacity", .06).attr("width", x(c) - 60).lower();
      top.selectAll(".tc").data([c]).join("text").attr("class", "tc w-tag fuerte").attr("x", x(c)).attr("y", 14).attr("text-anchor", "middle").text("corte: horas = " + f(c, 1));
      bot.selectAll(".mk").data([ev]).join("circle").attr("class", "mk").attr("r", 8).attr("fill", COL.arena).attr("stroke", "#fff").attr("stroke-width", 2).attr("cx", x(c)).attr("cy", yG(ev.gan));
      hijos(ev, `horas < ${f(c, 1)}`, `horas ≥ ${f(c, 1)}`);
      rd.innerHTML = `Ganancia (reducción de Gini) <b class="rojo">${f(ev.gan, 4)}</b>` + (Math.abs(c - mejorH.c) < 0.01 ? `<div class="verde"><b>¡Es el mejor corte posible! (el mismo que elige rpart)</b></div>` : "");
    }
    const yG = d3.scaleLinear([0, mejorH.gan * 1.15], [180, 10]);
    function montarHoras() {
      top.selectAll("*").remove(); bot.selectAll("*").remove();
      top.append("text").attr("class", "w-tag fuerte").attr("x", 60).attr("y", 50).attr("fill", COL.verde).text("correcto");
      top.append("text").attr("class", "w-tag fuerte").attr("x", 60).attr("y", 175).attr("fill", COL.rojo).text("incorrecto");
      top.append("g").selectAll("circle").data(D).join("circle").attr("r", 4.5).attr("opacity", .6).attr("fill", d => ok(d) ? COL.verde : COL.rojo)
        .attr("cx", d => x(d.horas)).attr("cy", d => (ok(d) ? 60 : 185) + d._j * 70);
      ejes(top, x, null, { y0: 262, sinY: true });
      ejes(bot, x, yG, { y0: 180, x0: 60, ylab: "Ganancia", xlab: "Punto de corte (horas)", fy: v => f(v, 2), ty: 3 });
      bot.append("path").attr("d", d3.line().x(e => x(e.c)).y(e => yG(e.gan))(curvaH)).attr("fill", "none").attr("stroke", COL.azul).attr("stroke-width", 2.5);
      bot.append("circle").attr("cx", x(mejorH.c)).attr("cy", yG(mejorH.gan)).attr("r", 6).attr("fill", COL.rojo);
      bot.append("text").attr("class", "w-tag rojo fuerte").attr("x", x(mejorH.c) + 10).attr("y", yG(mejorH.gan) - 4).text("máximo: " + f(mejorH.c, 1));
      dibujaHoras(desl.v);
    }
    function montarCat(v) {
      top.selectAll("*").remove(); bot.selectAll("*").remove();
      const C = cat[v], xb = d3.scaleBand(C.niv.map(d => d.k), [60, 860]).padding(0.25), yb = d3.scaleLinear([0, 1], [260, 30]);
      ejes(top, xb, yb, { y0: 260, x0: 60, ylab: "% correcto", fy: d3.format(".0%") });
      top.selectAll(".b").data(C.niv).join("rect").attr("class", "b").attr("x", d => xb(d.k)).attr("width", xb.bandwidth()).attr("y", 260).attr("height", 0)
        .attr("fill", d => C.mejor.izq.includes(d.k) ? COL.verde : COL.rojo).transition().duration(600).attr("y", d => yb(d.p)).attr("height", d => 260 - yb(d.p));
      top.selectAll(".t").data(C.niv).join("text").attr("class", "t w-tag fuerte").attr("x", d => xb(d.k) + xb.bandwidth() / 2).attr("y", d => yb(d.p) - 8).attr("text-anchor", "middle").text(d => `${pct(d.p)} (n=${d.n})`);
      const sx = xb(C.mejor.der[0]) - xb.step() * xb.paddingInner() / 2;
      top.append("line").attr("x1", sx).attr("x2", sx).attr("y1", 20).attr("y2", 275).attr("stroke", COL.tinta).attr("stroke-width", 3).attr("stroke-dasharray", "6 4");
      top.append("text").attr("class", "w-tag").attr("x", 60).attr("y", 300).text("rpart ordena las categorías por su % de acierto y prueba cada corte entre ellas");
      const yy = d3.scaleBand(C.ops.map(o => o.izq.join(", ") + " | " + o.der.join(", ")), [10, 180]).padding(0.25), xx = d3.scaleLinear([0, mejorH.gan * 1.05], [330, 860]);
      bot.selectAll("rect").data(C.ops).join("rect").attr("x", 330).attr("y", o => yy(o.izq.join(", ") + " | " + o.der.join(", "))).attr("height", yy.bandwidth()).attr("fill", o => o === C.mejor ? COL.arena : COL.claro).attr("width", o => xx(Math.max(0, o.gan)) - 330);
      bot.selectAll("text").data(C.ops).join("text").attr("class", "w-tag").attr("x", 320).attr("text-anchor", "end").attr("y", o => yy(o.izq.join(", ") + " | " + o.der.join(", ")) + yy.bandwidth() / 2 + 5).text(o => `{${o.izq.join(", ")}} vs {${o.der.join(", ")}}  ·  ${f(o.gan, 4)}`);
      hijos(C.mejor, `${v} ∈ {${C.mejor.izq.join(", ")}}`, `${v} ∈ {${C.mejor.der.join(", ")}}`);
      rd.innerHTML = `Mejor ganancia con <b>${v}</b>: <b class="rojo">${f(C.mejor.gan, 4)}</b>`;
    }
    function montarTodas() {
      top.selectAll("*").remove(); bot.selectAll("*").remove(); caja.innerHTML = "";
      const vs = Object.keys(gan).sort((a, b) => gan[b] - gan[a]), y = d3.scaleBand(vs, [30, 420]).padding(0.25), xx = d3.scaleLinear([0, mejorH.gan * 1.1], [160, 860]);
      ejes(top, xx, null, { y0: 430, sinY: true, xlab: "Mejor reducción de Gini de cada variable", fx: v => f(v, 2) });
      top.selectAll("rect").data(vs).join("rect").attr("x", 160).attr("y", v => y(v)).attr("height", y.bandwidth()).attr("width", 0).attr("fill", (v, i) => i ? COL.gris : COL.rojo)
        .transition().duration(800).delay((v, i) => i * 150).attr("width", v => xx(gan[v]) - 160);
      top.selectAll(".n").data(vs).join("text").attr("class", "n w-tag fuerte").attr("x", 150).attr("y", v => y(v) + y.bandwidth() / 2 + 6).attr("text-anchor", "end").text(v => v);
      top.selectAll(".g").data(vs).join("text").attr("class", "g w-tag").attr("x", v => xx(gan[v]) + 8).attr("y", v => y(v) + y.bandwidth() / 2 + 6).text(v => f(gan[v], 4));
      rd.innerHTML = `Gana <b class="rojo">horas</b> (corte en ${f(mejorH.c, 1)}): será la <b>primera pregunta</b> del árbol, la raíz.<div class="peq">Después, el algoritmo repite lo mismo dentro de cada hijo.</div>`;
    }
    function monta() { desl.inp.closest(".w-ctrl").style.display = modo === "horas" ? "" : "none"; ({ horas: montarHoras, todas: montarTodas }[modo] || (() => montarCat(modo)))(); }
    segmentado(panel, "Variable", [["horas", "horas"], ["nivel", "nivel"], ["L1", "L1"], ["estancia", "estancia"], ["todas", "Comparar"]], "horas", v => { modo = v; monta(); });
    desl = deslizador(panel, { etq: "Punto de corte", min: 0, max: cortes.length - 1, valor: 20, fmt: i => f(cortes[i], 1), cambio: i => dibujaHoras(cortes[i]) });
    desl.v; Object.defineProperty(desl, "v", { get() { return cortes[+desl.inp.value]; } });
    botones(panel, [{ etq: "Buscar el mejor corte", click: () => { if (modo !== "horas") return; let i = 0; const ib = cortes.indexOf(mejorH.c);
      const t = d3.interval(() => { desl.set(i, false); i += 3; if (i >= cortes.length) { t.stop(); setTimeout(() => desl.set(ib), 400); } }, 30); } }]);
    panel.appendChild(rd); panel.appendChild(caja); monta();
  };

  /* --- Árbol de rpart: crecer, explorar, seguir a un estudiante --- */
  function parseCond(c) {
    const m = c.match(/^(.+?)\s*(>=|<|=)\s*(.+)$/); if (!m) return null;
    return { v: m[1].trim(), op: m[2], val: m[2] === "=" ? m[3].split(",") : +m[3] };
  }
  function cumple(cond, a) {
    const p = parseCond(cond); if (!p) return true;
    if (p.op === ">=") return +a[p.v] >= p.val; if (p.op === "<") return +a[p.v] < p.val; return p.val.includes(String(a[p.v]));
  }
  const bonita = c => c.replace(">=", " ≥ ").replace("< ", " < ").replace(/(\d)\.(\d)/g, "$1,$2").replace("=", " = ").replace(/,(?=\S)/g, ", ").replace(/(\d), (\d)/g, "$1,$2");

  W.arbol = el => {
    const tipo = el.dataset.arbol || "grande", modo = el.dataset.modo || "explorar", nodos = CUR().arboles[tipo];
    const clase = nodos[0].clase !== undefined, mapa = new Map(nodos.map(n => [n.id, n]));
    const { vis, panel } = marco(el, { ancho: 400 }), w = 940, h = 560, svg = lienzo(vis, w, h);
    const raiz = d3.hierarchy(mapa.get(1), n => [mapa.get(2 * n.id), mapa.get(2 * n.id + 1)].filter(Boolean));
    const prof = raiz.height, bw = prof > 3 ? 118 : 160, bh = 56;
    // Disposición en orden: cada hoja ocupa su propia columna y cada padre se centra sobre sus hijos
    const hojas = raiz.leaves(), paso = (w - bw - 20) / Math.max(1, hojas.length - 1);
    hojas.forEach((d, i) => { d.x = bw / 2 + 10 + i * paso; });
    raiz.eachAfter(d => { if (d.children) d.x = d3.mean(d.children, c => c.x); });
    const dy = (h - bh - 50) / Math.max(1, prof);
    raiz.each(d => { d.y = bh / 2 + 34 + d.depth * dy; });
    const n0 = nodos[0].n, devRaiz = nodos[0].dev;
    const colorN = clase ? d3.scaleLinear([0, 0.5, 1], [COL.rojo, "#eeeeee", COL.verde]) : d3.scaleSequential(d3.extent(nodos, n => n.media), d3.interpolateRgb("#e3eef8", COL.azul));
    const gl = svg.append("g"), gt = svg.append("g"), gn = svg.append("g");
    const links = gl.selectAll("path").data(raiz.links()).join("path").attr("fill", "none").attr("stroke", COL.gris).attr("stroke-width", 2.5)
      .attr("d", l => `M${l.source.x},${l.source.y + bh / 2}C${l.source.x},${(l.source.y + l.target.y) / 2} ${l.target.x},${(l.source.y + l.target.y) / 2} ${l.target.x},${l.target.y - bh / 2}`);
    const ltxt = gt.selectAll("text").data(raiz.links()).join("text").attr("class", "w-tag rama").attr("text-anchor", "middle")
      .attr("x", l => l.target.x).attr("y", l => l.target.y - bh / 2 - 7).text(l => bonita(l.target.data.cond));
    const g = gn.selectAll("g").data(raiz.descendants()).join("g").attr("transform", d => `translate(${d.x},${d.y})`).style("cursor", "pointer");
    g.append("rect").attr("x", -bw / 2).attr("y", -bh / 2).attr("width", bw).attr("height", bh).attr("rx", 10).attr("stroke", "#fff").attr("stroke-width", 2);
    const t1 = g.append("text").attr("class", "nodo1").attr("text-anchor", "middle").attr("y", -5), t2 = g.append("text").attr("class", "nodo2").attr("text-anchor", "middle").attr("y", 16);
    const pinta = (d, hoja) => {
      const n = d.data;
      if (clase) { const pc = n.p, pr = n.clase === "correcto" ? pc : 1 - pc; return [n.clase, `${pct(pr)} ${n.clase === "correcto" ? "acierta" : "falla"} · n = ${n.n}`]; }
      return [`nota ${f(n.media, 1)}`, `n = ${n.n} (${pct(n.n / n0)})`];
    };
    g.select("rect").attr("fill", d => clase ? colorN(d.data.p) : colorN(d.data.media));
    t1.text(d => pinta(d)[0]); t2.text(d => pinta(d)[1]);
    g.selectAll("text").attr("fill", d => (clase ? Math.abs(d.data.p - 0.5) > 0.22 : d.data.media > 65) ? "#fff" : COL.tinta);
    const rd = lectura(panel, "grande");
    const regla = d => d.ancestors().reverse().slice(1).map(a => bonita(a.data.cond));
    function infoNodo(d) {
      const n = d.data, r = regla(d);
      return `<div class="peq">${d.depth ? "SI" : "Todos los estudiantes"}</div>` + (r.length ? `<div class="regla">${r.map(x => `<span>${x}</span>`).join(" <b>y</b> ")}</div>` : "") +
        `<div class="peq">ENTONCES</div>` +
        (clase ? `<div>predicción <b style="color:${n.clase === "correcto" ? COL.verde : COL.rojo}">${n.clase}</b></div><div class="barra2"><span style="width:${100 * n.p}%"></span></div>
          <div class="peq">${n.conteos[0]} correctos · ${n.conteos[1]} incorrectos (n = ${n.n}, ${pct(n.n / n0)} de la muestra)</div>`
          : `<div>nota media prevista <b class="rojo">${f(n.media, 1)}</b></div><div class="peq">n = ${n.n} (${pct(n.n / n0)} de la muestra)</div>`) +
        (n.hoja ? "" : `<div class="peq">Nodo interno: se divide por <b>${n.var}</b>.</div>`);
    }
    function resaltar(cam) {
      const s = new Set(cam);
      g.transition().duration(300).attr("opacity", d => !cam.length || s.has(d) ? 1 : 0.3);
      links.transition().duration(300).attr("stroke", l => s.has(l.target) ? COL.arena : COL.gris).attr("stroke-width", l => s.has(l.target) ? 7 : 2.5);
      ltxt.attr("font-weight", l => s.has(l.target) ? 800 : 400);
    }
    if (modo === "crecer") {
      let k = 0;
      const frontera = k => raiz.descendants().filter(d => d.depth === k ? true : d.depth < k && !d.children);
      function dibuja() {
        g.transition().duration(600).attr("opacity", d => d.depth <= k ? 1 : 0).attr("transform", d => `translate(${d.x},${d.y}) scale(${d.depth <= k ? 1 : 0.6})`);
        links.transition().duration(600).attr("opacity", l => l.target.depth <= k ? 1 : 0); ltxt.transition().duration(600).attr("opacity", l => l.target.depth <= k ? 1 : 0);
        const fr = frontera(k);
        if (clase) { const acc = d3.sum(fr, d => d3.max(d.data.conteos)) / n0;
          rd.innerHTML = `<div>Profundidad <b>${k}</b> · <b>${fr.length}</b> hojas</div><div>Acierto en estos datos <b class="rojo">${pct(acc, 1)}</b></div><div class="peq">${k === 0 ? "Sin preguntas: predecimos siempre la clase mayoritaria." : k === prof ? "Árbol completo. ¿De verdad necesitamos tantas preguntas? (lo veremos con la poda)" : "Cada nueva pregunta afina la predicción… en estos datos."}</div>`; }
        else { const r2 = 1 - d3.sum(fr, d => d.data.dev) / devRaiz;
          rd.innerHTML = `<div>Profundidad <b>${k}</b> · <b>${fr.length}</b> hojas</div><div>R² en estos datos <b class="rojo">${f(r2, 2)}</b></div>`; }
      }
      botones(panel, [{ etq: "← Podar", cls: "sec", click: () => { k = Math.max(0, k - 1); dibuja(); } }, { etq: "Crecer →", click: () => { k = Math.min(prof, k + 1); dibuja(); } }]);
      panel.appendChild(rd); dibuja();
      g.on("mouseenter", (e, d) => { if (d.depth <= k) rd.innerHTML = infoNodo(d); });
      return { enter() { k = 0; dibuja(); } };
    }
    if (modo === "explorar") {
      g.on("mouseenter click", (e, d) => { rd.innerHTML = infoNodo(d); resaltar(d.ancestors()); });
      svg.on("mouseleave", () => resaltar([]));
      rd.innerHTML = "Pasa el ratón por un nodo: verás la <b>regla completa</b> que lleva hasta él.";
      return {};
    }
    // modo seguir
    const a = { horas: 300, nivel: "B1", L1: "chino", estancia: "no" };
    const ficha = svg.append("g").attr("opacity", 0); ficha.append("circle").attr("r", 16).attr("fill", COL.arena).attr("stroke", "#fff").attr("stroke-width", 4);
        function seguir() {
      let d = raiz; const cam = [d];
      while (d.children) { const izq = d.children[0]; d = cumple(izq.data.cond, a) ? izq : d.children[1]; cam.push(d); }
      resaltar(cam);
      ficha.interrupt().attr("opacity", 1).attr("transform", `translate(${raiz.x - bw / 2 - 4},${raiz.y})`);
      let tr = ficha; cam.slice(1).forEach(c => { tr = tr.transition().duration(450).attr("transform", `translate(${c.x - bw / 2 - 4},${c.y})`); });
      const usadas = new Set(nodos.filter(n => !n.hoja).map(n => n.var));
      rd.innerHTML = infoNodo(d) + (["estancia", "nivel", "L1"].filter(v => !usadas.has(v)).length ?
        `<div class="peq nota-arbol">Variables que el árbol no usa: <b>${["estancia", "nivel", "L1"].filter(v => !usadas.has(v)).join(", ")}</b>. Cambiarlas no altera la predicción.</div>` : "");
    }
    deslizador(panel, { etq: "Horas de instrucción", min: 20, max: 520, paso: 5, valor: a.horas, cambio: v => { a.horas = v; seguir(); } });
    if (clase) selector(panel, "Nivel", NIVELES, a.nivel, v => { a.nivel = v; seguir(); });
    selector(panel, "Lengua materna (L1)", L1S, a.L1, v => { a.L1 = v; seguir(); });
    selector(panel, "Estancia", ["no", "sí"], a.estancia, v => { a.estancia = v; seguir(); });
    panel.appendChild(rd); seguir();
    return { enter: seguir };
  };

  W["cp-curva"] = el => {
    const T0 = CUR().cptabla, { vis, panel } = marco(el, { ancho: 400 }), w = 860, h = 470, svg = lienzo(vis, w, h);
    const errRaiz = 116 / 240, acc = e => 1 - e * errRaiz, x = d3.scaleLinear([1, d3.max(T0, d => d.nsplit + 1)], [70, 830]), y = d3.scaleLinear([0.45, 0.95], [410, 20]);
    ejes(svg.append("g"), x, y, { y0: 410, xlab: "Tamaño del árbol (número de hojas)", ylab: "Acierto", fy: d3.format(".0%") });
    const min = T0.reduce((a, b) => b.xerror < a.xerror ? b : a), umbral = min.xerror + min.xstd, unoEE = T0.find(d => d.xerror <= umbral);
    svg.append("path").attr("d", d3.area().x(d => x(d.nsplit + 1)).y0(d => y(acc(d.xerror - d.xstd))).y1(d => y(acc(d.xerror + d.xstd)))(T0)).attr("fill", COL.rojo).attr("opacity", 0.12);
    svg.append("line").attr("x1", 70).attr("x2", 830).attr("y1", y(acc(umbral))).attr("y2", y(acc(umbral))).attr("stroke", COL.tinta).attr("stroke-dasharray", "4 5");
    svg.append("text").attr("class", "w-tag").attr("x", 826).attr("y", y(acc(umbral)) + 18).attr("text-anchor", "end").text("mejor acierto − 1 EE (margen de incertidumbre)");
    [["rel_error", COL.azul, "acierto con los datos de entrenamiento (ya conocidos)"], ["xerror", COL.rojo, "acierto estimado con estudiantes nuevos (validación cruzada)"]].forEach(([k, c, t], i) => {
      svg.append("path").attr("d", d3.line().x(d => x(d.nsplit + 1)).y(d => y(acc(d[k])))(T0)).attr("fill", "none").attr("stroke", c).attr("stroke-width", 3.5);
      svg.append("g").selectAll("circle").data(T0).join("circle").attr("cx", d => x(d.nsplit + 1)).attr("cy", d => y(acc(d[k]))).attr("r", 5).attr("fill", c);
      svg.append("rect").attr("x", 300).attr("y", 350 + i * 26).attr("width", 18).attr("height", 5).attr("fill", c); svg.append("text").attr("class", "w-tag").attr("x", 326).attr("y", 357 + i * 26).text(t);
    });
    const mk = svg.append("line").attr("y1", 20).attr("y2", 410).attr("stroke", COL.arena).attr("stroke-width", 4);
    const rd = lectura(panel, "grande"); let s;
    function dibuja(i) {
      const d = T0[i]; mk.transition().duration(300).attr("x1", x(d.nsplit + 1)).attr("x2", x(d.nsplit + 1));
      rd.innerHTML = `<div><b>${d.nsplit + 1}</b> hojas · cp = ${f(d.cp, 4)}</div><div>Acierto en entrenamiento <b style="color:${COL.azul}">${pct(1 - d.rel_error * errRaiz, 1)}</b></div>
        <div>Acierto estimado con datos nuevos <b class="rojo">${pct(1 - d.xerror * errRaiz, 1)}</b></div>` +
        (d === unoEE ? `<div class="verde"><b>Regla de 1 EE: el árbol más pequeño casi tan bueno como el mejor.</b></div>` : d === min ? `<div class="verde"><b>Mínimo error de validación cruzada.</b></div>` :
          d.nsplit > min.nsplit ? `<div class="peq rojo">Sobreajuste: mejora en entrenamiento, empeora con datos nuevos.</div>` : "");
    }
    s = deslizador(panel, { etq: "Fila de la tabla cp", min: 0, max: T0.length - 1, valor: T0.length - 1, fmt: i => `${T0[i].nsplit + 1} hojas`, cambio: dibuja });
    botones(panel, [{ etq: "Elegir con la regla de 1 EE", click: () => s.set(T0.indexOf(unoEE)) }]);
    panel.appendChild(rd); dibuja(T0.length - 1);
  };

  W.confusion = el => {
    const H = CUR().holdout, { vis, panel } = marco(el, { ancho: 400 }), w = 860, h = 470, svg = lienzo(vis, w, h), st = { a: "grande", d: "prueba" };
    const m = svg.append("g").attr("transform", "translate(130,70)"), cs = 150;
    m.append("text").attr("class", "w-tag fuerte").attr("x", cs).attr("y", -40).attr("text-anchor", "middle").text("Predicción del árbol");
    ["correcto", "incorrecto"].forEach((t, i) => { m.append("text").attr("class", "w-tag").attr("x", cs / 2 + i * cs).attr("y", -12).attr("text-anchor", "middle").text(t);
      m.append("text").attr("class", "w-tag").attr("x", -12).attr("y", cs / 2 + i * cs + 5).attr("text-anchor", "end").text(t); });
    m.append("text").attr("class", "w-tag fuerte").attr("transform", "rotate(-90)").attr("x", -cs).attr("y", -100).attr("text-anchor", "middle").text("Realidad");
    const def = [{ k: "vp", i: 0, j: 0, t: "Verdaderos positivos", e: "Usan bien el subjuntivo y el árbol acierta." }, { k: "fn", i: 0, j: 1, t: "Falsos negativos", e: "Lo usan bien, pero el árbol dice «incorrecto»." },
      { k: "fp", i: 1, j: 0, t: "Falsos positivos", e: "Lo usan mal, pero el árbol dice «correcto»." }, { k: "vn", i: 1, j: 1, t: "Verdaderos negativos", e: "Lo usan mal y el árbol acierta." }];
    const cel = m.selectAll("g").data(def).join(en => { const g = en.append("g"); g.append("rect"); g.append("text").attr("class", "celda grande").attr("text-anchor", "middle"); return g; })
      .attr("transform", d => `translate(${d.j * cs},${d.i * cs})`).style("cursor", "pointer");
    cel.select("rect").attr("width", cs - 6).attr("height", cs - 6).attr("rx", 12); cel.select("text").attr("x", (cs - 6) / 2).attr("y", (cs - 6) / 2 + 12);
    const b = svg.append("g").attr("transform", "translate(520,60)");
    b.append("text").attr("class", "w-tag fuerte").attr("x", 0).attr("y", -20).text("Acierto: entrenamiento vs prueba");
    const arb = ["grande", "podado", "simple"], yb = d3.scaleBand(arb, [0, 300]).padding(0.3), xb = d3.scaleLinear([0.5, 1], [90, 320]);
    const acc = (a, d) => { const c = H[a][d]; return (c.vp + c.vn) / (c.vp + c.vn + c.fp + c.fn); };
    arb.forEach(a => { b.append("text").attr("class", "w-tag fuerte").attr("x", 80).attr("y", yb(a) + yb.bandwidth() / 2 + 5).attr("text-anchor", "end").text(`${a} (${H[a].hojas})`);
      ["entreno", "prueba"].forEach((d, k) => { const yy = yb(a) + k * yb.bandwidth() / 2;
        b.append("rect").attr("x", 90).attr("y", yy).attr("height", yb.bandwidth() / 2 - 2).attr("width", xb(acc(a, d)) - 90).attr("fill", k ? COL.rojo : COL.azul).attr("opacity", .85);
        b.append("text").attr("class", "w-tag").attr("x", xb(acc(a, d)) + 6).attr("y", yy + yb.bandwidth() / 4 + 4).text(pct(acc(a, d))); }); });
    b.append("text").attr("class", "w-tag").attr("x", 90).attr("y", 330).attr("fill", COL.azul).text("■ entrenamiento"); b.append("text").attr("class", "w-tag").attr("x", 230).attr("y", 330).attr("fill", COL.rojo).text("■ prueba");
    const rd = lectura(panel, "grande"), ex = lectura(panel);
    function dibuja() {
      const c = H[st.a][st.d === "prueba" ? "prueba" : "entreno"], tot = c.vp + c.vn + c.fp + c.fn;
      cel.select("rect").transition().duration(400).attr("fill", d => (d.k === "vp" || d.k === "vn") ? COL.verde : COL.rojo).attr("opacity", d => 0.25 + 0.75 * c[d.k] / tot * 2.2);
      cel.select("text").text(d => c[d.k]);
      rd.innerHTML = `<div>Exactitud <b>${pct((c.vp + c.vn) / tot, 1)}</b></div><div>Sensibilidad <b>${pct(c.vp / (c.vp + c.fn))}</b> <span class="peq">(correctos detectados)</span></div>
        <div>Especificidad <b>${pct(c.vn / (c.vn + c.fp))}</b> <span class="peq">(incorrectos detectados)</span></div><div class="peq">${tot} estudiantes de ${st.d === "prueba" ? "prueba (nunca vistos)" : "entrenamiento"} · árbol ${st.a} (${H[st.a].hojas} hojas)</div>`;
    }
    cel.on("mouseenter click", (e, d) => { ex.innerHTML = `<b>${d.t}</b>: ${d.e}`; });
    segmentado(panel, "Árbol", [["grande", "Sin podar"], ["podado", "Podado (mín. xerror)"], ["simple", "1 corte"]], "grande", v => { st.a = v; dibuja(); });
    segmentado(panel, "Datos", [["entreno", "Entrenamiento"], ["prueba", "Prueba"]], "prueba", v => { st.d = v; dibuja(); });
    panel.appendChild(rd); panel.appendChild(ex); dibuja();
  };

  /* =====================================================================
     ARRANQUE
     ===================================================================== */
  function anatomias() {
    document.querySelectorAll(".anatomia").forEach(box => {
      const panel = box.querySelector(".an-panel"), spans = [...box.querySelectorAll(".an")]; if (!panel || !spans.length) return;
      const muestra = s => { spans.forEach(o => o.classList.toggle("on", o === s)); panel.innerHTML = s.dataset.exp; };
      spans.forEach(s => { s.addEventListener("mouseenter", () => muestra(s)); s.addEventListener("click", () => muestra(s)); });
      const b = document.createElement("button"); b.className = "w-btn"; b.textContent = "Recorrer la salida →"; let i = -1;
      b.onclick = () => { i = (i + 1) % spans.length; muestra(spans[i]); }; panel.after(b);
    });
  }
  // Botón «Copiar» en los recuadros de código de los componentes (el código se puede pegar en RStudio)
  function copiar(texto, boton) {
    const hecho = () => { boton.textContent = "¡Copiado!"; boton.classList.add("ok"); setTimeout(() => { boton.textContent = "Copiar"; boton.classList.remove("ok"); }, 1600); };
    const plan_b = () => { const t = document.createElement("textarea"); t.value = texto; t.style.position = "fixed"; t.style.opacity = "0";
      document.body.appendChild(t); t.select(); try { document.execCommand("copy"); hecho(); } catch (e) { boton.textContent = "Selecciona y copia"; } t.remove(); };
    if (navigator.clipboard && window.isSecureContext) navigator.clipboard.writeText(texto).then(hecho, plan_b); else plan_b();
  }
  function botonesCopiar(raiz) {
    if (!raiz || !raiz.querySelectorAll) return;
    const pres = raiz.matches && raiz.matches("pre.w-code") ? [raiz] : [...raiz.querySelectorAll("pre.w-code")];
    pres.forEach(pre => {
      if (pre.parentElement && pre.parentElement.classList.contains("w-code-caja")) return;
      const caja = document.createElement("div"); caja.className = "w-code-caja"; pre.replaceWith(caja); caja.appendChild(pre);
      const b = document.createElement("button"); b.type = "button"; b.className = "w-copiar"; b.textContent = "Copiar"; b.title = "Copiar el código";
      b.addEventListener("click", e => { e.stopPropagation(); copiar(pre.textContent.replace(/\u00a0/g, " "), b); }); caja.appendChild(b);
    });
  }
  function contadores(slide) {
    slide.querySelectorAll(".contador[data-hasta]").forEach(c => {
      const fin = +c.dataset.hasta; d3.select(c).transition().duration(1400).ease(d3.easeCubicOut).tween("t", () => { const i = d3.interpolateNumber(0, fin); return t => { c.textContent = Math.round(i(t)); }; });
    });
  }
  // Construye los componentes de una diapositiva (solo la primera vez)
  function construir(slide) {
    if (!slide) return;
    slide.querySelectorAll(".w[data-w]").forEach(el => {
      const fn = W[el.dataset.w]; if (!fn || el._w) return;
      try { el._w = fn(el) || {}; botonesCopiar(el); } catch (e) { el._w = {}; console.error("[curso] Error en el componente", el.dataset.w, e); el.innerHTML = `<p class="pequeno">No se pudo cargar «${el.dataset.w}».</p>`; }
    });
  }
  // Prepara las diapositivas vecinas cuando el navegador queda libre
  const enReposo = window.requestIdleCallback || (f => setTimeout(f, 200));
  function vecinas(R) {
    const ss = R.getSlides(), i = ss.indexOf(R.getCurrentSlide());
    enReposo(() => [1, -1, 2].forEach(d => construir(ss[i + d])));
  }
  function iniciar() {
    if (!window.CURSO) { console.warn("[curso] Faltan los datos (interactivo/datos_curso.js)"); return; }
    document.querySelectorAll(".practica input[type=checkbox], .reto input[type=checkbox]").forEach(c => { c.disabled = false; });
    document.querySelectorAll(".giro > div").forEach(c => c.addEventListener("click", () => c.classList.toggle("on")));
    anatomias();
    // Los componentes que reescriben su código (p. ej., el árbol para elegir la prueba) reciben también el botón
    const slidesEl = document.querySelector(".reveal .slides");
    if (slidesEl && window.MutationObserver) new MutationObserver(ms => ms.forEach(m => m.addedNodes.forEach(n => { if (n.nodeType === 1) botonesCopiar(n); })))
      .observe(slidesEl, { childList: true, subtree: true });
    const R = window.Reveal;
    if (R) {
      // En lugar de construir los ~70 componentes al cargar (lento en móvil y tableta),
      // se construyen al llegar a cada diapositiva y se adelantan las vecinas.
      const entrar = s => { if (!s) return; construir(s); s.querySelectorAll(".w").forEach(el => el._w && el._w.enter && el._w.enter()); contadores(s); };
      R.on("slidechanged", e => { entrar(e.currentSlide); window.dispatchEvent(new Event("resize")); vecinas(R); });
      entrar(R.getCurrentSlide());
      vecinas(R);
    } else {
      document.querySelectorAll("section").forEach(construir);
    }
  }
  if (window.Reveal && window.Reveal.isReady && window.Reveal.isReady()) iniciar();
  else if (window.Reveal && window.Reveal.on) window.Reveal.on("ready", iniciar);
  else window.addEventListener("load", () => { if (window.Reveal && !window.Reveal.isReady()) window.Reveal.on("ready", iniciar); else iniciar(); });
})();
