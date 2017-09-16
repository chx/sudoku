# this is an earlier attempt, not used.

defmodule Sudoku.Units do
  # This is a structure and there are no default values.
  defstruct []
  def fetch(map, key), do: :maps.find(key, map)
end

defimpl Collectable, for: Sudoku.Units do
  def into(original) do
    {original, fn
      # v will become a double list like [[ 'A1', 'A2', ...]]
      # so concatenating will result in [['A1', 'A2', ..], ['B1', 'B2', ...]]
      source, {:cont, [k | v ]} -> Map.update(source, k, v, &(v ++ &1))
      source, :done -> source
    end}
  end
end
  #@units    for s <- @squares, u <- @unitlist, s in u, into: %Units{}, do: [s, u]
