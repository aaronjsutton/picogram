defmodule Mix.Tasks.Picogram do
  use Mix.Task

  @shortdoc "Picogram mix interface for Phoenix projects"

  @moduledoc """
    Deploy a release.
  """

  @doc false
  def run(args) do
    case args do
      [host, port, user_dir, rel_root] -> 
        :application.ensure_all_started(:picogram)
        :application.ensure_all_started(:ssh)
        {port, ""} = Integer.parse(port)
        :picogram_cli.main([{:host, to_charlist(host)}, {:port, port}, {:user_dir, to_charlist(user_dir)}, {:release_root, to_charlist(rel_root)}, {:install_strategy, 'phoenix'}])
      _ ->
        IO.puts("bad args.")
    end
  end
end
