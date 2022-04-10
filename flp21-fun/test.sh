#!/usr/bin/env bash
info_passed=0
info_failed=0
rrg_passed=0
rrg_failed=0
nfa_passed=0
nfa_failed=0

mkdir -p "outs"
for f in tests/*.in ; do
  f_out=outs/$(basename "$f" .in).out_info
  f_exp=exps/$(basename "$f" .in).exp_info
  ret=$(./flp21-fun -i "$f" > "$f_out" 2>&1) ;
  diff=$(diff "$f_exp" "$f_out")
  if [[ "$ret" -eq 0  || "$diff" -eq 0 ]]; then
    info_passed="$((info_passed + 1))"
  else
    echo "$f" failed on info
    info_failed="$((info_failed + 1))"
  fi
done
echo Info passed: "$info_passed"
echo Info failed: "$info_failed"

for f in tests/*.in ; do
  f_out=outs/$(basename "$f" .in).out_rrg
  f_exp=exps/$(basename "$f" .in).exp_rrg
  ret=$(./flp21-fun -1 "$f" > "$f_out" 2>&1) ;
  diff=$(diff "$f_exp" "$f_out")
  if [[ "$ret" -eq 0  || "$diff" -eq 0 ]]; then
    rrg_passed="$((rrg_passed + 1))"
  else
    echo "$f" failed on RRG
    rrg_failed="$((rrg_failed + 1))"
  fi
done
echo RRG passed: "$rrg_passed"
echo RRG failed: "$rrg_failed"

for f in tests/*.in ; do
  f_out=outs/$(basename "$f" .in).out_nfa
  f_exp=exps/$(basename "$f" .in).exp_nfa
  ret=$(./flp21-fun -2 "$f" > "$f_out" 2>&1) ;
  diff=$(diff "$f_exp" "$f_out")
  if [[ "$ret" -eq 0  || "$diff" -eq 0 ]]; then
    nfa_passed="$((nfa_passed + 1))"
  else
    echo "$f" failed on NFA
    nfa_failed="$((nfa_failed + 1))"
  fi
done
echo NFA passed: "$nfa_passed"
echo NFA failed: "$nfa_failed"
