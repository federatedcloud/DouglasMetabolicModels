function outMap = incOrAdd(m, k)
  if m.isKey
    m(k) = m(k) + 1;
  else
    m(k) = 1;
  end
end
