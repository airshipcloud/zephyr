create table "objects"("id" bigint, "path" hstore, "value" hstore);

create index "objects_path" on "objects" using gist ("path");
