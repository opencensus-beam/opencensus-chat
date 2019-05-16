defmodule Chat.History do
  def get() do
    url = Application.get_env(:chat, Chat.History)[:url]

    case :httpc.request(:get, {url, get_tracing_headers() ++ [{'Content-Type', 'application/json'}]}, [], []) do
      {:ok, {{'HTTP/1.1', 200, 'OK'}, _headers, body}} ->
        body

      {:ok, {{'HTTP/1.1', 204, 'No Content'}, _headers, _}} ->
        "[]"

      _ -> {:error, "Connection error"}
    end
  end

  def add(data) when is_binary(data) do
    url = Application.get_env(:chat, Chat.History)[:url]

    :httpc.request(:post, {url, get_tracing_headers(), 'application/json', to_string(data)}, [], [])
  end

#

  defp get_tracing_headers() do
    case :ocp.current_span_ctx() do
      :undefined -> []
      context ->
        :oc_propagation_http_tracecontext.to_headers(context)
        |> Enum.map(fn ({k, v}) ->
            {:erlang.binary_to_list(k), :erlang.binary_to_list(List.to_string(v))}
          end)
    end
  end
end