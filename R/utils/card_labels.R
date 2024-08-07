card_labels <- tibble::tribble(
  ~name, ~label,
  # champion
  'Golden Knight',     'GoldKnt',  
  'Archer Queen',      'Queen',    
  'Skeleton King',     'SkelKing', 
  'Mighty Miner',      'MiMiner',
  'Monk',              'Monk',
  'Little Prince',     'LilPrince',
  
  # legendary
  'Princess',          'Princess', 
  'The Log',           'Log',      
  'Miner',             'Miner',    
  'Ice Wizard',        'Ice Wiz',  
  'Bandit',            'Bandit',   
  'Night Witch',       'NW',       
  'Royal Ghost',       'Ghost',    
  'Electro Wizard',    'EWiz',     
  'Mega Knight',       'MK',       
  'Inferno Dragon',    'I Dragn',  
  'Magic Archer',      'M Archr',  
  'Lumberjack',        'Lmbr J',   
  'Lava Hound',        'Hound',    
  'Graveyard',         'GrvYard',  
  'Sparky',            'Sparky',   
  'Ram Rider',         'R Rider',  
  'Fisherman',         'Fisher', 
  'Mother Witch',      'MotherW',
  'Phoenix',           'Phoenix',
  'Goblin Machine',    'GobMach',
  
  # epic
  'Goblin Barrel',     'Gob Bar',  
  'Golem',             'Golem',    
  'Poison',            'Poison',   
  'Executioner',       'Exo',      
  'P.E.K.K.A',         'Pekka',    
  'Lightning',         'Lighting', 
  'Baby Dragon',       'Baby D',   
  'Electro Dragon',    'E Drag',   
  'Tornado',           'Tornado',  
  'Hunter',            'Hunter',   
  'Dark Prince',       'Dark P',   
  'Prince',            'Prince',   
  'Witch',             'Witch',    
  'Mirror',            'Mirror',   
  'Guards',            'Guards',   
  'Skeleton Army',     'Skarmy',   
  'X-Bow',             'XBow',     
  'Freeze',            'Freeze',   
  'Clone',             'Clone',    
  'Giant Skeleton',    'Giant Sk', 
  'Bowler',            'Bowler',   
  'Balloon',           'Balloon',  
  'Rage',              'Rage',     
  'Cannon Cart',       'C Cart',   
  'Barbarian Barrel',  'Barb Bar', 
  'Goblin Giant',      'Gob Giant',
  'Wall Breakers',     'Wall Brk', 
  'Electro Giant',     'E Giant', 
  'Goblin Drill',      'GobDrill',
  'Void',              'Void',
  'Goblin Curse',      'GobCurse',
  
  # rare
  'Hog Rider',         'Hog',      
  'Fireball',          'Fireball', 
  'Mega Minion',       'Mega Min', 
  'Musketeer',         'Musk',     
  'Battle Ram',        'Bat Ram',  
  'Elixir Golem',      'E Golem',  
  'Battle Healer',     'Healer',   
  'Three Musketeers',  '3M',       
  'Rocket',            'Rocket',   
  'Royal Hogs',        'Royal Hog',
  'Valkyrie',          'Valk',     
  'Inferno Tower',     'Inf Tower',
  'Elixir Collector',  'Elixir',   
  'Ice Golem',         'Ice Golem',
  'Goblin Hut',        'Gob Hut',  
  'Giant',             'Giant',    
  'Mini P.E.K.K.A',    'Mini Peka',
  'Tombstone',         'Tombstone',
  'Furnace',           'Furnance', 
  'Wizard',            'Wizard',   
  'Zappies',           'Zappies',  
  'Flying Machine',    'F Machine',
  'Barbarian Hut',     'Barb Hut', 
  'Dart Goblin',       'Dart Gob', 
  'Heal Spirit',       'Heal Spr', 
  'Bomb Tower',        'B Tower',  
  'Earthquake',        'Quake',    
  'Goblin Cage',       'Gob Cage', 
  'Goblin Demolisher', 'Gob Demo',
  'Suspicious Bush',   'Bush',
  
  # common
  'Zap',               'Zap',      
  'Goblin Gang',       'Gob Gang', 
  'Mortar',            'Mortar',   
  'Bats',              'Bats',     
  'Royal Giant',       'Roy Giant',
  'Ice Spirit',        'Ice Sprit',
  'Firecracker',       'Firecrack',
  'Bomber',            'Bomber',   
  'Skeletons',         'Skeleton', 
  'Tesla',             'Tesla',    
  'Spear Goblins',     'Spear Gob',
  'Cannon',            'Cannon',   
  'Archers',           'Archer',   
  'Barbarians',        'Barbs',    
  'Goblins',           'Goblin',   
  'Fire Spirit',       'Fire Spr', 
  'Knight',            'Knight',   
  'Skeleton Barrel',   'Skel Bar', 
  'Elite Barbarians',  'E Barb',   
  'Arrows',            'Arrows',   
  'Minions',           'Minion',   
  'Minion Horde',      'Min Horde',
  'Giant Snowball',    'Snowball', 
  'Royal Recruits',    'Recruits', 
  'Royal Delivery',    'Royal Del',
  'Rascals',           'Rascals',  
  'Skeleton Dragons',  'Skel Drag',
  'Electro Spirit',    'E Spirit'
)

make_master_card_list <- function() {
  pl <- cr_get_player('JYJQC88')
  pl$cards[[1]] |> 
    select(
      name, max_level, image = icon_urls_medium
    ) |> 
    left_join(card_labels, by = 'name') |> 
    mutate(label = ifelse(is.na(label), name, label))
}
