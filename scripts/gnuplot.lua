local system = require 'pandoc.system'

local gnuplot_template = [[
    set term svg background '#000000' size 800, 500
    set output "docs/%s"

    # line styles for ColorBrewer Pastel1
    # for use with qualitative/categorical data
    # provides 8 pale colors based on Set1
    # compatible with gnuplot >=4.2
    # author: Anna Schneider
    
    # line styles
    set style line 1 lc rgb '#FBB4AE' # pale red
    set style line 2 lc rgb '#B3CDE3' # pale blue
    set style line 3 lc rgb '#CCEBC5' # pale green
    set style line 4 lc rgb '#DECBE4' # pale purple
    set style line 5 lc rgb '#FED9A6' # pale orange
    set style line 6 lc rgb '#FFFFCC' # pale yellow
    set style line 7 lc rgb '#E5D8BD' # pale brown
    set style line 8 lc rgb '#FDDAEC' # pale pink
    
    # palette
    rcol(x) = 0.237 - 2.13*x + 26.92*x**2 - 65.5*x**3 + 63.5*x**4 - 22.36*x**5
    gcol(x) = ((0.572 + 1.524*x - 1.811*x**2)/(1 - 0.291*x + 0.1574*x**2))**2
    bcol(x) = 1/(1.579 - 4.03*x + 12.92*x**2 - 31.4*x**3 + 48.6*x**4 - 23.36*x**5)
    set palette model RGB functions rcol(gray), gcol(gray), bcol(gray)

    set style line 101 lc rgb '#f0a040' lt 1 lw 1
    set border 3 front ls 101
    set key textcolor rgb '#f0a040'
    %s
]]

function CodeBlock(block)
    if block.classes[1] == "gnuplot" and block.attributes["output"] ~= nil then
        local caption, rawsrc = block.text:match("(.-)\n%-%-%-\n(.*)")
        if not caption then
            rawsrc = block.text
            caption = ""
        end
        outfile = block.attributes["output"]
        system.with_temporary_directory("run-gnuplot", function (tmpdir)
            local src = gnuplot_template:format(outfile, rawsrc)
            local f = io.open(tmpdir .. "/plot.gp", "w")
            f:write(src)
            f:close()
            os.execute("mkdir -p docs/$(dirname " .. outfile ..")")
            os.execute("gnuplot " .. tmpdir .. "/plot.gp")
        end)
        return pandoc.Para({pandoc.Image({pandoc.Str(caption)}, outfile, caption, {class = "figure"})})
    end
end

