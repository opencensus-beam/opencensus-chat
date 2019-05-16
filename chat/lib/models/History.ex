defmodule Chat.History do
  def get() do
    :ocp.with_child_span("Chat.History.get")

    url = Application.get_env(:chat, Chat.History)[:url]

    result = case :httpc.request(:get, {url, get_tracing_headers() ++ [{'Content-Type', 'application/json'}]}, [], []) do
      {:ok, {{'HTTP/1.1', 200, 'OK'}, _headers, body}} ->
        body

      {:ok, {{'HTTP/1.1', 204, 'No Content'}, _headers, _}} ->
        "[]"

      _ -> {:error, "Connection error"}
    end

    :ocp.finish_span()

    result
  end

  def add(data) when is_binary(data) do
    :ocp.with_child_span("Chat.History.add")

    url = Application.get_env(:chat, Chat.History)[:url]

    :httpc.request(:post, {url, get_tracing_headers(), 'application/json', to_string(data)}, [], [])

    :ocp.finish_span()
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