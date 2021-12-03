local system = require 'pandoc.system'

local gnuplot_template = [[
    set term svg background '#000000' size 500, 300
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
    set palette maxcolors 8
    set palette defined ( 0 '#FBB4AE',\
        	    	      1 '#B3CDE3',\
    		      2 '#CCEBC5',\
    		      3 '#DECBE4',\
    		      4 '#FED9A6',\
    		      5 '#FFFFCC',\
    		      6 '#E5D8BD',\
    		      7 '#FDDAEC' )
    set style line 101 lc rgb '#f0a040' lt 1 lw 1
    set border 3 front ls 101
    set key textcolor rgb '#f0a040'
    %s
]]

function CodeBlock(block)
    if block.classes[1] == "gnuplot" then
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
        return pandoc.Para({pandoc.Image({pandoc.Str(caption)}, outfile)})
    end
end

