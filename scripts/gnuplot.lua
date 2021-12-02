local system = require 'pandoc.system'

local gnuplot_template = [[
    set term svg
    set output "_site/content/%s"
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
            os.execute("mkdir -p _site/content/$(dirname " .. outfile ..")")
            os.execute("gnuplot " .. tmpdir .. "/plot.gp")
        end)
        return pandoc.Para({pandoc.Image({pandoc.Str(caption)}, "../../" .. outfile)})
    end
end

