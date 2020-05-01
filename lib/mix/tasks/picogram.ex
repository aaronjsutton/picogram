defmodule Mix.Tasks.Picogram do
  use Mix.Task

  @shortdoc "Picogram mix interface"

  @moduledoc """
    Deploy a release.
  """

  @doc false
  def run(args) do
    case args do
      [] -> 
        :application.ensure_all_started(:picogram)
        :application.ensure_all_started(:ssh)
        :picogram_cli.main([:print_version, :connect], [{:host, 'localhost'}, {:port, 4000}, {:user_dir, 'priv/user'}])
    end
  end
end
