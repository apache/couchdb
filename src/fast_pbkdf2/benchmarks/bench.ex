defmodule PBKDF2.Benchmarks do
  def pbkdf2_input itCount do
    password = :base64.encode(:crypto.strong_rand_bytes(10))
    salt = :base64.encode(:crypto.strong_rand_bytes(16))
    {password, salt, itCount}
  end

  def pbkdf2 {password, salt, it_count} do
    :fast_pbkdf2.pbkdf2(:sha256, password, salt, it_count)
  end
end

{:ok, :erl_pbkdf2} = :compile.file('./test/erl_pbkdf2.erl')

Benchee.run(
  %{
    "fast_pbkdf2" => fn {password, salt, it_count} ->
        :fast_pbkdf2.pbkdf2(:sha256, password, salt, it_count)
      end,
    "erl_pbkdf2 " => fn {password, salt, it_count} ->
        :erl_pbkdf2.pbkdf2_oneblock(:sha256, password, salt, it_count)
      end
  },
  inputs: %{
    "1. 8" => PBKDF2.Benchmarks.pbkdf2_input(8),
    "2. 512" => PBKDF2.Benchmarks.pbkdf2_input(512),
    "3. 4096" => PBKDF2.Benchmarks.pbkdf2_input(4096),
    "4. 10000" => PBKDF2.Benchmarks.pbkdf2_input(10000),
    "5. 160000" => PBKDF2.Benchmarks.pbkdf2_input(160000),
    "6. 500000" => PBKDF2.Benchmarks.pbkdf2_input(500000)
  },
  parallel: 12,
  time: 5,
  memory_time: 5
)

