function code_name(block)
    if block.attributes["file"] ~= nil then
        return "file:" .. block.attributes["file"]
    end
    if block.identifier == nil or block.identifier == "" then
        return nil
    else
        return "«" .. block.identifier .. "»"
    end
end

function CodeBlock(block)
    local name = code_name(block)
    if name ~= nil then
        return pandoc.Div({ pandoc.Para({ pandoc.Str(name) })
                          , pandoc.CodeBlock(block.text, {class = block.classes[1]})
                          }, { class="named-code-block" })
    end
end
