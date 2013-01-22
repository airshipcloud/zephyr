create table "objects"("id" bigint not null, "path" hstore not null, "value" hstore not null);

create index "objects_path" on "objects" using gist ("path");
create index "objects_value" on "objects" using gist ("value");
