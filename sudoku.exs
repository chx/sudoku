defmodule Sudoku.Helpers do
  # wrapping in a list is necessary to make it work with single characters
  # and charlists both.
  def cross(a, b) do
    for x <- List.wrap(a), y <- List.wrap(b), do: {x, y}
  end
  # this is for testing and inspecting.
  def to_list(a) when is_list(a), do: Enum.map(a, &to_list/1)
  def to_list(a) when is_map(a), do: for {k, v} <- a, into: %{}, do: {to_list(k), to_list(v)}
  def to_list(a) when is_tuple(a), do: to_list(Tuple.to_list(a))
  def to_list(a), do: a
end

defmodule Sudoku do
  import Sudoku.Helpers

  @digits   '123456789'
  @rows     'ABCDEFGHI'
  @cols     @digits
  @squares  cross(@rows, @cols)

  @unitlist (for c <- @cols, do: cross(@rows, c)) ++
            (for r <- @rows, do: cross(r, @cols)) ++
            (for rs <- ['ABC', 'DEF', 'GHI'], cs <- ['123', '456', '789'], do: cross(rs, cs))

  @units    for s <- @squares, into: %{}, do: {s, (for u <- @unitlist, s in u, do: u)}

  @peers    for s <- @squares, into: %{},
                               do: {s,
                                 @units[s]
                                 |> List.flatten()
                                 |> Enum.uniq()
                                 |> List.delete(s)}

  def parse_grid(grid) do
    values = for s <- @squares, into: %{}, do: {s, @digits}
    Enum.zip(@squares, grid)
    |> Enum.reduce(values, &assign/2)
  end

  defp assign({s, d}, values) when d in @digits, do: Enum.reduce(values[s] -- [d], values, &(eliminate(s, &1, &2)))

  defp assign(_, values), do: values

  defp eliminate(s, d, values), do: eliminate(s, d, values, d in values[s])

  # Already eliminated
  defp eliminate(_, _, values, false), do: values

  defp eliminate(s, d, values, true) do
    # Eliminate d from values[s]
    Map.update(values, s, nil, &(&1 -- [d]))
    |> eliminate_from_peers(s)
    |> eliminate_from_units(s, d)
  end

  # (1) If a square s is reduced to one value, then eliminate it from the peers.
  defp eliminate_from_peers(values, s) do
    case values[s] do
      [] -> throw :contradiction
      [h | []] -> Enum.reduce(@peers[s], values, &(eliminate(&1, h, &2)))
      _ -> values
    end
  end

  # (2) If a unit u is reduced to only one place for a value d, then put it there.
  defp eliminate_from_units(values, s, d) do
    Enum.reduce(
      @units[s],
      values,
      fn u, values ->
        case Enum.filter(u, &(d in values[&1])) do
          [] -> throw :contradiction
          [h | []] -> assign({h, d}, values)
          _ -> values
        end
      end
    )
  end

  defp value_length({_, value}), do: length(value)

  defp unsolved(values), do: Enum.filter(values, &(value_length(&1) > 1))

  defp finished(values), do: {(if Enum.empty?(unsolved(values)), do: :halt, else: :cont), values}

  def search(values), do: search(values, unsolved(values))

  defp search(values, []), do: {:halt, values}

  defp search(values, unsolved) do
    {s, digits} = Enum.min_by(unsolved, &value_length/1)
    Enum.reduce_while(
      digits,
      values,
      fn d, values ->
        try do
          search(assign({s, d}, values))
        catch
          :contradiction -> {:cont, values}
        end
      end
    )
    |> finished
  end

  def display(values) do
    {_, longest} = Enum.max_by(values, &value_length/1)
    width = length(longest) + 1
    square_line = String.duplicate("-", width * 3 + 1)
                  |> to_charlist()
    full_line = ["\n"] ++ square_line ++ '+' ++ square_line ++ '+' ++ square_line
    for r <- @rows do
      ' '
      ++
      (for c <- @cols do
         values[{r, c}]
         ++ (if c in '36', do: ' |', else: '')
       end
       |> Enum.intersperse(' ')
        ) ++ (if r in 'CF', do: full_line, else: '')
    end
    |> Enum.intersperse("\n")
  end

  def test do
    81 = length(@squares)
    27 = length(@unitlist)
    true = Enum.all?(for s <- @squares, do: length(@units[s]) == 3)
    true = Enum.all?(for s <- @squares, do: length(@peers[s]) == 20)
    units_c2 = [
      ['A2', 'B2', 'C2', 'D2', 'E2', 'F2', 'G2', 'H2', 'I2'],
      ['C1', 'C2', 'C3', 'C4', 'C5', 'C6', 'C7', 'C8', 'C9'],
      ['A1', 'A2', 'A3', 'B1', 'B2', 'B3', 'C1', 'C2', 'C3']
    ]
    true = Enum.sort(to_list(@units)['C2']) == Enum.sort(units_c2)
    peers_c2 = [
      'A2', 'B2', 'D2', 'E2', 'F2', 'G2', 'H2', 'I2',
      'C1', 'C3', 'C4', 'C5', 'C6', 'C7', 'C8', 'C9',
      'A1', 'A3', 'B1', 'B3']
    true = Enum.sort(to_list(@peers)['C2']) == Enum.sort(peers_c2)
    IO.puts "tests passed"
  end
end

Sudoku.test
grid1 = '003020600900305001001806400008102900700000008006708200002609500800203009005010300'
{_, values} = Sudoku.parse_grid(grid1)
|> Sudoku.search
values
|> Sudoku.display
|> IO.puts

grid2 = '4.....8.5.3..........7......2.....6.....8.4......1.......6.3.7.5..2.....1.4......'
{_, values} = Sudoku.parse_grid(grid2)
|> Sudoku.search
values
|> Sudoku.display
|> IO.puts
