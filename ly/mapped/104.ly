\version "2.14.2"
\include "util.ly"
\header{ tagline = ""}
\paper {
  print-all-headers = ##t
  ragged-right = ##f
  %print-all-headers = ##t
  paper-height = 11\in
  paper-width = 8.5\in
  indent = 0\in
  %system-system-spacing = #'((basic-distance . 10) (padding . 0))
  system-system-spacing =
    #'((basic-distance . 0)
       (minimum-distance . 0)
       (padding . 2)
       (stretchability . 100))
  score-markup-spacing =
    #'((basic-distance . 0)
       (minimum-distance . 0)
       (padding . -14)
       (stretchability . 60))
  top-system-spacing =
    #'((basic-distance . 0)
       (minimum-distance . 0)
       (padding . -1.5)
       (stretchability . 0))
  ragged-last-bottom = ##f
  ragged-bottom = ##f
  two-sided = ##t
  inner-margin = 1\in
  outer-margin = 0.75\in
  top-margin = 0.26\in
  bottom-margin = 0.25\in
  first-page-number = #104
  print-first-page-number = ##t
  headerLine = ""
  oddHeaderMarkup = \markup\fill-line{
     \override #'(font-name . "Garamond Premier Pro")\abs-fontsize #12.5
     \combine 
        \fill-line{"" \on-the-fly #print-page-number-check-first
        \oldStylePageNum""
        }
        \fill-line{\headerLine}
  }
  evenHeaderMarkup = \markup {
     \override #'(font-name . "Garamond Premier Pro")\abs-fontsize #12.5
     \combine
        \on-the-fly #print-page-number-check-first
        \oldStylePageNum""
        \fill-line{\headerLine}
  }
}
#(set-global-staff-size 18) \paper{ #(define fonts (make-pango-font-tree "Garamond Premier Pro" "Garamond Premier Pro" "Garamond Premier Pro" (/ 18 20))) }
global = {
  \key ees \major
  \time 2/2
  \dynamicUp
  %\set crescendoSpanner = #'dashed-line
  %\set midiInstrument = "recorder"
  \autoBeamOff
  \override DynamicTextSpanner #'style = #'none
  \slurDashed
  \tieDashed
}

sopMusic = \relative c' {
	\partial 8
  bes'8 |
  ees~ ees ees4 d ees8~ ees |
  c8~ c bes4 g ees8~ ees |
  f~ f f~ f ees4 d4 |
  ees2. bes'4 |
  
  c4 d4 ees c8~ c |
  f~ f d4 bes bes8~ bes |
  ees~ ees ees4 d8~ d c4 |
  bes2. bes8~ bes |
  
  ees~ ees ees~ ees d4 ees8~ ees |
  c~ c bes4 g bes8~ bes |
  c4 bes8 bes aes~ aes g4 |
  f-> g-> aes->\fermata bes8~ bes |
  
  bes~ bes bes4 c c8~ c |
  bes4 g4 ees ees8~ ees |
  f4 f8~ f ees~ ees d4 |
  ees2 \bar"||"
  
  %Chorus
  bes'2 |
  ees,4. ees8 ees4 g |
  bes2 b4\rest bes |
  c4. bes8 c4 d |
  ees2 b4\rest bes |
  ees ees d ees |
  
  c bes g bes |
  c bes aes g |
  f-> g-> aes->\fermata bes |
  ees,4 ees8 ees ees4 g |
  bes2 b4\rest bes |
  
  c4. bes8 c4 d |
  ees2 bes4\rest bes |
  ees4 ees8 ees d4 ees8 ees |
  c4 bes g ees |
  f ees ees4. d8 |
  ees2. \bar"|."
}
sopWords = \lyricmode {
  \set stanza = #"1. "
  \set ignoreMelismata = ##t
	A cap -- it -- al ship for an o -- _ cean trip
  Was the Wal -- lop -- ing _ Win -- dow Blind!
  No wind that blew dis -- _ mayed _ her crew,
  Or __ _ troub -- led the cap -- _ tain’s mind
  
  The _ man _ at the wheel was __ _ made _ to feel
  Con -- _ tempt for the wild -- _ est blow -- ow -- ow,
  Tho’ it of -- ten ap -- peared when the gale had clear’d,
  That he’d been in his bunk _ be -- low.
}

sopWordsII = \lyricmode {
  \set stanza = #"2. "
  \set ignoreMelismata = ##t
  The bo’ -- _ swain’s mate was _ ver -- y se -- date,
  Yet __ _ fond _ of a -- muse -- ment too;
  He played hop -- scotch with the star -- _ board watch
  While the cap -- _ tain tick -- led the crew.
  
  And the gun -- ner we __ _ had was ap -- par -- ent -- ly mad
  For he stood on the can -- _ non’s tai -- ai -- ail,
  And _ fired _ sal -- utes in the cap -- tain’s boots
  In the teeth of a boom -- _ ing gale.
  
  \unset ignoreMelismata
  Then blow, ye winds, heigh ho!
  A rov -- ing I will go!
  I’ll stay no more on Eng -- land’s shore,
  So let the mu -- sic play -- ay -- ay!
  I’m off for the morn -- ing train!
  I’ll cross the rag -- ing main!
  I’m off to my love with a box -- ing glove,
  Ten thou -- sand miles a -- way!
}

sopWordsIII = \lyricmode {
  \set stanza = #"3. "
  \set ignoreMelismata = ##t
  The cap -- _ tain sat in a com -- mo -- dore’s hat
  And __ _ dined _ in a roy -- al way
  On toast -- ed pigs and __ _ pick -- les and figs
  And __ _ gum -- mer -- y bread _ each day.
  
  But the rest of us __ _ ate from an o -- di -- ous plate
  For the food that was giv -- en the crew -- ew -- ew
  Was a num -- ber of tons of __ _ hot cross buns
  Served _ up with _ su -- gar and glue.
}

sopWordsIV = \lyricmode {
  \set stanza = #"4. "
}

sopWordsV = \lyricmode {
  \set stanza = #"5. "
}

altoMusic = \relative c' {
  g'8 |
  g~ g g4 f g8~ g |
  ees~ ees ees4 ees ees8~ ees |
  d8~ d d~ d bes4 bes |
  ees2. g4 |
  
  ees4 f f f8~ f |
  d'~ d bes4 g g8~ g |
  g~ g g4 f8~ f ees4 |
  d2. d8~ d |
  
  g~ g g~ g f4 g8~ g |
  ees~ ees ees4 ees ees8~ ees |
  ees4 ees8 ees d~ d ees4 |
  d4 ees f d8~ d |
  
  ees~ ees ees4 ees ees8~ ees |
  ees4 ees ees ees8~ ees |
  d4 d8~ d bes~ bes bes4 |
  bes2 \bar"||"
  
  %Chorus
  d2 |
  ees4. ees8 ees4 ees |
  ees2 s4 g |
  aes4. bes8 aes4 aes |
  g2 s4 g |
  g g f g |
  
  ees ees ees g |
  aes bes d, ees |
  d ees f d |
  ees4 ees8 ees ees4 ees |
  ees2 s4 g |
  
  aes4. bes8 aes4 aes |
  g2 s4 bes |
  g g8 g aes4 bes8 bes |
  aes4 ees ees8[ d] c4 |
  c c bes4. bes8 |
  bes2. \bar"|."
}
altoWords = \lyricmode {
}
altoWordsII = \lyricmode {
%\markup\italic
  \set stanza = #"2. "
}
altoWordsIII = \lyricmode {
  \set stanza = #"3. "
}
altoWordsIV = \lyricmode {
  \set stanza = #"4. "
}
altoWordsV = \lyricmode {
  \set stanza = #"5. "
  \set ignoreMelismata = ##t
}
altoWordsVI = \lyricmode {
  \set stanza = #"6. "
  \set ignoreMelismata = ##t
}
tenorMusic = \relative c' {
  ees8 |
  bes~ bes bes4 bes bes8~ bes |
  aes~ aes bes4 bes g8~ g |
  aes~ aes aes~ aes g4 f |
  g2. bes4 |
  
  c bes c c8~ c |
  f8~ f bes,4 d d8~ d |
  ees~ ees ees4 bes8~ bes a4 |
  bes2. bes8~ bes |
  
  bes~ bes bes~ bes bes4 bes8~ bes |
  aes~ aes bes4 bes bes8~ bes |
  c4 bes8 bes bes~ bes bes4 |
  bes bes bes bes8~ bes |
  
  g~ g g4 aes aes8~ aes |
  g4 bes g g8~ g |
  aes4 aes8~ aes g~ g f4 |
  g2 \bar"||"
  
  %Chorus
  bes2 |
  g4. g8 g4 bes |
  bes2 s4 ees |
  ees4. ees8 ees4 bes |
  bes2 s4 bes |
  ees ees d ees |
  
  c bes bes ees |
  ees ees bes bes |
  bes bes bes bes |
  g g8 g g4 bes |
  bes2 s4 ees |
  
  ees4. ees8 ees4 bes |
  bes2 s4 bes |
  bes bes8 bes bes4 ees8 ees |
  ees4 bes bes g |
  aes aes g4. aes8 |
  g2. \bar"|."
}

tenorWords = \lyricmode {
}

tenorWordsII = \lyricmode {
}

tenorWordsIII = \lyricmode {
}

bassMusic = \relative c' {
  ees,8 |
  ees8~ ees ees4 bes ees8~ ees |
  aes~ aes g[ f] ees4 ees8~ ees |
  bes~ bes bes~ bes bes4 bes |
  ees2. ees4 |
  
  aes4 aes a a8~ a |
  bes~ bes bes4 g g8~ g |
  ees8~ ees ees4 f8~ f f4 |
  bes4( aes g) f8~ f |
  
  ees8~ ees ees~ ees bes4 ees8~ ees |
  aes8~ aes g4 ees g8~ g |
  aes4 g8 g f~ f ees4 |
  bes-> bes-> bes-> bes8~ bes |
  
  ees~ ees ees4 aes, aes8~ aes |
  ees'4 ees ees ees8~ ees |
  bes4 bes8~ bes bes~ bes bes4 |
  ees2 \bar"||"
  
  %Chorus
  bes2 |
  ees4. ees8 ees4 ees |
  g2 d4\rest g |
  aes4. g8 aes4 f |
  ees2 d4\rest ees |
  bes' bes bes bes |
  
  aes g ees ees |
  aes g f ees |
  bes bes bes\fermata bes |
  ees4 ees8 ees ees4 ees |
  g2 d4\rest g |
  
  aes4. g8 aes4 f |
  ees2 d4\rest bes' |
  ees, ees8 ees f4 g8 g |
  aes4 g ees8[ bes] c4 |
  aes aes bes4. bes8 |
  ees2. \bar"|."
}
bassWords = \lyricmode {
}

pianoRH = \relative c' {
}
pianoLH = \relative c' {
}

\score {
<<
   \new ChoirStaff <<
    \new Staff = women <<
      \new Voice = "sopranos" { \voiceOne << \global \sopMusic >> }
      \new Voice = "altos" { \voiceTwo << \global \altoMusic >> }
    >>
    \new Lyrics = "altos"  \lyricsto "sopranos" \sopWords
    \new Lyrics = "altosII"  \lyricsto "sopranos" \sopWordsII
    \new Lyrics = "altosIII"  \lyricsto "sopranos" \sopWordsIII
    \new Lyrics = "altosIV"  \lyricsto "sopranos" \sopWordsIV
    \new Lyrics = "altosV"  \lyricsto "sopranos" \sopWordsV
   \new Staff = men <<
      \clef bass
      \new Voice = "tenors" { \voiceOne << \global \tenorMusic >> }
      \new Voice = "basses" { \voiceTwo << \global \bassMusic >> }
    >>
    \new Lyrics \with { alignAboveContext = #"tenors" } \lyricsto "tenors" \tenorWordsIII
    \new Lyrics \with { alignAboveContext = #"tenors" } \lyricsto "tenors" \tenorWordsII
    \new Lyrics \with { alignAboveContext = #"tenors" } \lyricsto "tenors" \tenorWords
  >>
%    \new PianoStaff << \new Staff { \new Voice { \pianoRH } } \new Staff { \clef "bass" \pianoLH } >>
  >>
  
  \layout {
    \context {
      \Lyrics
      \override LyricText #'font-size = #1.3
      \override VerticalAxisGroup #'staff-affinity = #0
      \override LyricText #'X-offset = #center-on-word
    }
    \context {
      \Score
      \override SpacingSpanner #'base-shortest-duration = #(ly:make-moment 1 8)
      \override SpacingSpanner #'common-shortest-duration = #(ly:make-moment 1 4)
    }
    \context {
      % Remove all empty staves
      \Staff \RemoveEmptyStaves \override VerticalAxisGroup #'remove-first = ##t
      
      \override VerticalAxisGroup #'staff-staff-spacing =
      #'((basic-distance . 0)
         (minimum-distance . 0)
         (padding . -1)
         (stretchability . 2))
    }
  }
\header {
  title = \markup{\override #'(font-name . "Garamond Premier Pro Semibold"){ \abs-fontsize #18 \smallCapsOldStyle"A Capital Ship"}}
  poet = \markup\oldStyleNum"Charles E. Carryl (1841–1920)"
  composer = \markup\oldStyleNum"English Folk Song"
  tagline = ""
}}


global = {
  \key g \major
  \time 3/4
  \dynamicUp
  %\set crescendoSpanner = #'dashed-line
  %\set midiInstrument = "recorder"
  \autoBeamOff
  \override DynamicTextSpanner #'style = #'none
}

sopMusic = \transpose bes g {
  \relative c' {
    bes'4 bes c |
    a4. bes8 c4 |
    d d ees |
    d4. c8 bes4 |
    c bes a |
    
    bes2. |
    f'4 f f |
    f4. ees8 d4 |
    ees ees ees |
    ees4. d8 c4 |
    d ees8[ d] c[ bes] |
    
    %page2
    d4. ees8 f4 |
    g8[^\markup\italic"rit." ees] d4 c |
    bes2.\bar"|."
  }
}
sopWords = \lyricmode {
  \set stanza = #"1. "
  My coun -- try, ’tis of thee,
  Sweet land of lib -- er -- ty,
  Of thee I sing;
  
  Land where my fa -- thers died,
  Land of the pil -- grims’ pride,
  From ev -- ’ry moun -- tain -- side
  Let free -- dom ring!
}

sopWordsII = \lyricmode {
  \set stanza = #"2. "
  My na -- tive coun -- try, thee,
  Land of the no -- ble free,
  Thy name I love;
  
  I love thy rocks and rills,
  Thy woods and tem -- pled hills;
  My hearts with rap -- ture thrills,
  Like that a -- bove.
}

sopWordsIII = \lyricmode {
  \set stanza = #"3. "
  Let mu -- sic swell the breeze,
  And ring from all the trees
  Sweet free -- dom’s song;
  
  Let mor -- tal tongues a -- wake;
  Let all that breathe par -- take;
  Let rocks their si -- lence break,
  The sound pro -- long.
}

sopWordsIV = \lyricmode {
  \set stanza = #"4. "
  Our fa -- thers’ God to Thee,
  Au -- thor of lib -- er -- ty,
  To Thee we sing.
  
  Long may our land be bright,
  With free -- dom’s ho -- ly light,
  Pro -- tect us by Thy might,
  Great God our King.
}

sopWordsV = \lyricmode {
  \set stanza = #"5. "
}

altoMusic = \relative c' {
  d4 e e |
  d4. e8 fis4 |
  g g g |
  g4. fis8 e4 |
  
  e4 d d |
  d2. |
  b'4 g b |
  b4. a8 g4 |
  fis a fis |
  
  a4. g8 fis4 |
  g4 fis8[ g] d8[ e] |
  g4. fis8 g4 |
  g4 g fis |
  g2. \bar"|."
}
altoWords = \lyricmode {
}
altoWordsII = \lyricmode {
%\markup\italic
  \set stanza = #"2. "
}
altoWordsIII = \lyricmode {
  \set stanza = #"3. "
}
altoWordsIV = \lyricmode {
  \set stanza = #"4. "
}
altoWordsV = \lyricmode {
  \set stanza = #"5. "
  \set ignoreMelismata = ##t
}
altoWordsVI = \lyricmode {
  \set stanza = #"6. "
  \set ignoreMelismata = ##t
}
tenorMusic = \relative c' {
  b4 b c |
  a4. a8 d4 |
  d e e |
  d4. c8 b4 |
  
  c4 b a |
  b2. |
  d4 d d |
  d4. fis8 d4 |
  d d d |
  
  d4. d8 d4 |
  d d4 c |
  d4. d8 b4 |
  c8[ e] d4 c |
  b2. \bar"|."
}

tenorWords = \lyricmode {
}

tenorWordsII = \lyricmode {
}

tenorWordsIII = \lyricmode {
}

bassMusic = \relative c' {
  g4 e c |
  d4. d8 d4 |
  g e c |
  d4. dis8 e4 |
  
  c d d |
  g,2. |
  g4 b d |
  g4. d8 g4 |
  d fis a |
  
  d,4. g8 d4 |
  g a8[ g] fis[ e] |
  d8[ c] b[ a] g4 |
  c4 d d |
  g,2. \bar"|."
}
bassWords = \lyricmode {
}

pianoRH = \relative c' {
}
pianoLH = \relative c' {
}

\score {
<<
   \new ChoirStaff <<
    \new Staff = women <<
      \new Voice = "sopranos" { \voiceOne << \global \sopMusic >> }
      \new Voice = "altos" { \voiceTwo << \global \altoMusic >> }
    >>
    \new Lyrics = "altos"  \lyricsto "sopranos" \sopWords
    \new Lyrics = "altosII"  \lyricsto "sopranos" \sopWordsII
    \new Lyrics = "altosIII"  \lyricsto "sopranos" \sopWordsIII
    \new Lyrics = "altosIV"  \lyricsto "sopranos" \sopWordsIV
    \new Lyrics = "altosV"  \lyricsto "sopranos" \sopWordsV
   \new Staff = men <<
      \clef bass
      \new Voice = "tenors" { \voiceOne << \global \tenorMusic >> }
      \new Voice = "basses" { \voiceTwo << \global \bassMusic >> }
    >>
    \new Lyrics \with { alignAboveContext = #"tenors" } \lyricsto "tenors" \tenorWordsIII
    \new Lyrics \with { alignAboveContext = #"tenors" } \lyricsto "tenors" \tenorWordsII
    \new Lyrics \with { alignAboveContext = #"tenors" } \lyricsto "tenors" \tenorWords
  >>
%    \new PianoStaff << \new Staff { \new Voice { \pianoRH } } \new Staff { \clef "bass" \pianoLH } >>
  >>
  
  \layout {
    \context {
      \Lyrics
      \override LyricText #'font-size = #1.3
      \override VerticalAxisGroup #'staff-affinity = #0
      \override LyricText #'X-offset = #center-on-word
    }
    \context {
      \Score
      \override SpacingSpanner #'base-shortest-duration = #(ly:make-moment 1 8)
      \override SpacingSpanner #'common-shortest-duration = #(ly:make-moment 1 4)
    }
    \context {
      % Remove all empty staves
      \Staff \RemoveEmptyStaves \override VerticalAxisGroup #'remove-first = ##t
      
      \override VerticalAxisGroup #'staff-staff-spacing =
      #'((basic-distance . 0)
         (minimum-distance . 0)
         (padding . -1)
         (stretchability . 2))
    }
  }
\header {
  title = \markup{\override #'(font-name . "Garamond Premier Pro Semibold"){ \abs-fontsize #18 \smallCapsOldStyle"America"}}
  poet = \markup\oldStyleNum"Samuel Francis Smith (1808–1895)"
  composer = \markup\oldStyleNum"Traditional"
  tagline = ""
}}


