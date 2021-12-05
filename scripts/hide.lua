function Set (list)
  local set = {}
  for _, l in ipairs(list) do set[l] = true end
  return set
end

function CodeBlock(block)
        if Set(block.classes)["hide"] then
                return pandoc.Null()
        end
end
