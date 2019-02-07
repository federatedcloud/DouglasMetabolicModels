function out = cellzip(in1, in2)
  sz1 = size(in1);
  sz2 = size(in2);
  if sz1 == sz2'
    in2 = in2';
    sz2 = sz2';
  end
  if sz1 ~= sz2
    error('sz1 != sz2 in cellzip')
  end

  flat1 = {in1{:}};
  flat2 = {in2{:}};
  clen = length(flat1);
  out = cell(clen,1);
  for ii = 1:clen
    out{ii} = {flat1{ii}, flat2{ii}};
  end
