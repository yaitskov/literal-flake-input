sudo su

cd /sys/fs/cgroup
mkdir lfi
cd lfi
echo 500M > memory.max
echo 250000 100000 > cpu.max
---
cgexec -g memory,cpu:lfi ./result/bin/literal-flake-input run > cq.log &
---
rm -rf l && tsung -l l  -f basic.xml start && cd l/* && tsung_stats.pl  && firefox report.html && cd -

# live monitoring
firefox http://localhost:8091

---
httperf --hog --server localhost --port 3042 --wsesslog 10000,10,http_scenario --rate 390 --timeout 5

httperf seems can make DoS
it turned out that there was mistake in tsung xml in target URL path


---
I disabled google monitoring due high resource usage (cpu 10%)
How about Haskell solution for monitoring?
https://maxgabriel.github.io/ekg-yesod/
