<h1 id="haskell-plot">Haskell-Plot</h1>
<p>I'm starting from scratch to write a new plotting library based on <a href="http://projects.haskell.org/diagrams/">Diagrams</a>. The old package can be found in the &quot;Old&quot; directory.</p>
<h1 id="header">Header</h1>
<pre class="sourceCode literate haskell"><code class="sourceCode haskell"><span class="ot">{-# LANGUAGE OverloadedStrings #-}</span>
<span class="ot">{-# LANGUAGE TemplateHaskell #-}</span>
<span class="ot">{-# LANGUAGE UnicodeSyntax #-}</span></code></pre>
<pre class="sourceCode literate haskell"><code class="sourceCode haskell"><span class="kw">import </span><span class="dt">Diagrams.Prelude</span>
<span class="kw">import </span><span class="dt">Diagrams.Backend.SVG</span>
<span class="kw">import </span><span class="dt">Data.Default</span>
<span class="kw">import </span><span class="dt">System.Random</span>
<span class="kw">import </span><span class="dt">Graphics.Rendering.HPlot</span></code></pre>
<h1 id="make-axis">Make axis</h1>
<p>Currently the <strong>Axis</strong> type contains three components:</p>
<ol style="list-style-type: decimal">
<li>point map</li>
<li>labels and their positions</li>
<li>the actual axis with type &quot;Diagram B R2&quot;</li>
</ol>
<p>We usually do not create <strong>Axis</strong> directly, instead, we creat a function which take a number (the length of axis) and generate the axis. Such functions are wrapped in the <strong>AxisFn</strong> type. <strong>AxisFn</strong>s are building blocks of chart. I wrote some general functions to help create <strong>AxisFn</strong>, i.e., realAxis, indexAxis, emptyAxis.</p>
<pre class="sourceCode literate haskell"><code class="sourceCode haskell">xs <span class="ot">∷</span> [<span class="dt">Double</span>]
xs <span class="fu">=</span> take <span class="dv">50</span> <span class="fu">$</span> randomRs (<span class="fu">-</span><span class="dv">100</span>, <span class="dv">100</span>) <span class="fu">$</span> mkStdGen <span class="dv">2</span>

ys <span class="ot">∷</span> [<span class="dt">Double</span>]
ys <span class="fu">=</span> take <span class="dv">50</span> <span class="fu">$</span> randomRs (<span class="fu">-</span><span class="dv">100</span>, <span class="dv">100</span>) <span class="fu">$</span> mkStdGen <span class="dv">4</span>

xAxis <span class="ot">∷</span> <span class="dt">AxisFn</span>
xAxis <span class="fu">=</span> realAxis (minimum xs, maximum xs) <span class="dv">0</span><span class="fu">.</span><span class="dv">2</span> def

yAxis <span class="ot">∷</span> <span class="dt">AxisFn</span>
yAxis <span class="fu">=</span> realAxis (minimum ys, maximum ys) <span class="dv">0</span><span class="fu">.</span><span class="dv">2</span> def</code></pre>
<p>The <strong>PlotArea</strong> contains four axes: left axis, top axis, right axis and bottom axis. We can use <strong>plotArea</strong> to create a <strong>PlotArea</strong>.</p>
<pre class="sourceCode literate haskell"><code class="sourceCode haskell">area <span class="ot">∷</span> <span class="dt">PlotArea</span>
area <span class="fu">=</span> plotArea <span class="dv">5</span><span class="fu">.</span><span class="dv">5</span> <span class="dv">4</span><span class="fu">.</span><span class="dv">8</span>
       ( yAxis  <span class="co">-- left axis</span>
       , def  <span class="co">-- top axis, using default axis which is a line</span>
       , def  <span class="co">-- right axis</span>
       , xAxis <span class="co">-- bottom axis</span>
       )</code></pre>
<p><strong>PlotArea</strong> can be converted to a Diagram by <strong>showPlot</strong>.</p>
<pre class="sourceCode literate haskell"><code class="sourceCode haskell">areaDiag <span class="ot">∷</span> <span class="dt">Diagram</span> <span class="dt">B</span> <span class="dt">R2</span>
areaDiag <span class="fu">=</span> (showPlot area) </code></pre>
<div class="figure">
<img src="doc/area.svg" />
</div>
<p>Now that we have the plotArea, we can start adding actual plots. For example, we can use points to make point plot:</p>
<pre class="sourceCode literate haskell"><code class="sourceCode haskell">ps <span class="fu">=</span> points xs ys def</code></pre>
<pre class="sourceCode literate haskell"><code class="sourceCode haskell">pointPlot <span class="fu">=</span> area <span class="fu">&lt;+</span> (ps, <span class="dt">BL</span>) <span class="co">-- attach plot to plot area according to bottom and left axes</span></code></pre>
<div class="figure">
<img src="doc/points.svg" />
</div>
<p>You can attach any number of plots to plotArea:</p>
<pre class="sourceCode literate haskell"><code class="sourceCode haskell">ls <span class="fu">=</span> line xs ys def
linePointPlot <span class="fu">=</span> area <span class="fu">&lt;+</span> (ps, <span class="dt">BL</span>) <span class="fu">&lt;+</span> (ls, <span class="dt">BL</span>)</code></pre>
<div class="figure">
<img src="doc/lp.svg" />
</div>
<p>You can create an indexed Axis by indexAxis:</p>
<pre class="sourceCode literate haskell"><code class="sourceCode haskell">bottomAxis <span class="fu">=</span> indexAxis <span class="dv">50</span> [] <span class="dv">0</span><span class="fu">.</span><span class="dv">2</span> def</code></pre>
<p>Now let's create a plot area with 3 axes: left, bottom and right</p>
<pre class="sourceCode literate haskell"><code class="sourceCode haskell">ys&#39; <span class="ot">∷</span> [<span class="dt">Double</span>]
ys&#39; <span class="fu">=</span> take <span class="dv">50</span> <span class="fu">$</span> randomRs (<span class="fu">-</span><span class="dv">1</span>, <span class="dv">1</span>) <span class="fu">$</span> mkStdGen <span class="dv">2</span>

yAxis&#39; <span class="fu">=</span> realAxis (minimum ys&#39;, maximum ys&#39;) <span class="dv">0</span><span class="fu">.</span><span class="dv">2</span> def

area&#39; <span class="fu">=</span> plotArea <span class="dv">5</span><span class="fu">.</span><span class="dv">5</span> <span class="dv">4</span><span class="fu">.</span><span class="dv">8</span> (yAxis, def, yAxis&#39;, bottomAxis)

l1 <span class="fu">=</span> line <span class="dt">Nothing</span> ys def <span class="st"># lc green</span>
l2 <span class="fu">=</span> line <span class="dt">Nothing</span> ys&#39; def <span class="st"># lc red</span>

plot <span class="fu">=</span> area&#39; <span class="fu">&lt;+</span> (l1, <span class="dt">BL</span>) <span class="fu">&lt;+</span> (l2, <span class="dt">BR</span>)</code></pre>
<div class="figure">
<img src="doc/doublePlot.svg" />
</div>
<p>Note that the green line is placed according to left axis, and the red line is placed according to right axis.</p>
<pre class="sourceCode literate haskell"><code class="sourceCode haskell">main <span class="fu">=</span> renderSVG <span class="st">&quot;doublePlot.svg&quot;</span> (<span class="dt">Dims</span> <span class="dv">480</span> <span class="dv">480</span>) <span class="fu">$</span> showPlot plot</code></pre>
