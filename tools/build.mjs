import * as esbuild from "esbuild";

const entry = `
import * as echarts from "echarts/core";
import { LineChart } from "echarts/charts";
import { GridComponent, TooltipComponent, LegendComponent, TitleComponent } from "echarts/components";
import { CanvasRenderer } from "echarts/renderers";
echarts.use([LineChart, GridComponent, TooltipComponent, LegendComponent, TitleComponent, CanvasRenderer]);
export { echarts };
`;

const result = await esbuild.build({
  stdin: { contents: entry, resolveDir: ".", loader: "js" },
  bundle: true,
  minify: true,
  format: "iife",
  globalName: "tsEcharts",
  outfile: "../inst/htmlwidgets/lib/echarts-line.min.js",
  metafile: true,
});

const bytes = Object.values(result.metafile.outputs)[0].bytes;
console.log(`Built echarts-line.min.js — ${(bytes / 1024).toFixed(0)} KB`);
