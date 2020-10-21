%reads the file 
complete = readmatrix('TestingReading.csv');
%finds what row the data stops at
[i j] = find(isnan(complete(:,2)));
%creating new vector with the data we want 
tofvec = complete(1:(i(1,1)-1),10);
idlist = complete(1:(i(1,1)-1),1);
%we save the ids that do not have the correct tof here
badid = zeros(size(idlist));
counter = 1; 
for h = 1:(i(1,1)-1)
    if tofvec(h,1) < 1000 | tofvec(h,1) > 3000
        badid(counter,1) = idlist(h,1);
        counter = counter + 1;
    end 
end