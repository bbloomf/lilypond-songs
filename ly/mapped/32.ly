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
  system-system-spacing =
    #'((basic-distance . 0)
       (minimum-distance . 0)
       (padding . -4)
       (stretchability . 100))
  score-markup-spacing =
    #'((basic-distance . 0)
       (minimum-distance . 0)
       (padding . 0.5)
       (stretchability . 100))
  top-system-spacing =
    #'((basic-distance . 0)
       (minimum-distance . 0)
       (padding . -1.5)
       (stretchability . 0))
  ragged-last-bottom = ##f
  ragged-last-bottom = ##f
  ragged-bottom = ##f
  two-sided = ##t
  inner-margin = 1\in
  outer-margin = 0.75\in
  top-margin = 0.26\in
  bottom-margin = 0.25\in
  first-page-number = #32
  print-first-page-number = ##t
  headerLine = ""
  oddHeaderMarkup = \markup\fill-line{
     \override #'(font-name . "Garamond Premr Pro")\abs-fontsize #12.5
     \combine 
        \fill-line{"" \on-the-fly #print-page-number-check-first
        \oldStylePageNum""
        }
        \fill-line{\headerLine}
  }
  evenHeaderMarkup = \markup {
     \override #'(font-name . "Garamond Premr Pro")\abs-fontsize #12.5
     \combine
        \on-the-fly #print-page-number-check-first
        \oldStylePageNum""
        \fill-line{\headerLine}
  }
}
#(set-global-staff-size 18) \paper{ #(define fonts (make-pango-font-tree "Garamond Premr Pro" "Garamond Premr Pro" "Garamond Premr Pro" (/ 18 20))) }
global = {
  \key g \major
  \time 4/4
  \dynamicUp
  %\set crescendoSpanner = #'dashed-line
  %\set midiInstrument = "recorder"
  \autoBeamOff
  \override DynamicTextSpanner #'style = #'none
}

sopMusic = \relative c' {
	d'4.. b16 a4 \times 2/3 {g8[ e d]} |
  g4 g g2 |
  d'4.. c16 b4 \times 2/3 {a8[ b d]} |
  e4 a, a2 |
  d4.. b16 a4 \times 2/3 {g8[ e d]} |
  
  g4 g g2 |
  b4. c8 d( g,4) c8 |
  \acciaccatura b a4.. g16 g4 b\rest |
  g4.. g16 a4 e |
  g g b8( d4.) |
  
  b4. b8 d[ c] b[ a] |
  g[ e] e[ g] a2\fermata |
  e'4 a, d g, |
  c fis, b( g8) b\rest |
  
  fis4.^\markup\italic"rall." g8 a4 \times 2/3 {b8[ cis d]} |
  fis,4 \acciaccatura fis8 e8.[ d16] d2\fermata |
  g4.^\markup{\dynamic"pp" \italic"a tempo"} e8 d4 e |
  g g a2 |
  c4.. c16 b4 a |
  
  d8[ b] a[ g] e2 |
  g4. e8 d4\cresc e\! |
  g4~ \times 2/3 {g8[ b d]} e2\fermata |
  g,4.\f a8 b( g'4)\fermata b,8 |
  a2 g4 b\rest \bar"|."
}
sopWords = \lyricmode {
  \set stanza = #"1. "
	By Kil -- lar -- ney’s lakes and fells,
  Em -- ’rald isles and wind -- ing bays,
  Moun -- tain paths and wood -- land dells,
  Mem -- ’ry ev -- er fond -- ly strays.
  Boun -- teous na -- ture loves all lands,
  Beau -- ty wan -- ders ev -- ’ry -- where,
  Foot -- prints leaves on ma -- ny strands,
  But her home is sure -- ly there!
  
  An -- gels fold their wings and rest,
  In that E -- den of the West,
  Beau -- ty’s home, Kil -- lar -- ney,
  Ev -- er fair Kil -- lar -- ney.
}

sopWordsII = \lyricmode {
  \set stanza = #"2. "
  In -- nis -- fal -- len’s ru -- ined shrine
  May sug -- gest a pass -- ing sigh;
  But man’s faith can ne’er de -- cline
  Such God’s won -- ders float -- ing by;
  Cas -- tle Lough and Glen -- a bay;
  Moun -- tains Tore and Ea -- gle’s Nest;
  Still at Mu -- cross you must pray
  Though the monks are now at rest.
  
  An -- gels won -- der not that man
  There would fain pro -- long life’s span,
  Beau -- ty’s home, Kil -- lar -- ney,
  Ev -- er fair Kil -- lar -- ney.
}

sopWordsIII = \lyricmode {
  \set stanza = #"3. "
  No place else can charm the eye
  With such bright and va -- ried tints,
  Ev -- ’ry rock that you pass by,
  Ver -- dure broid -- ers or be -- sprints.
  Vir -- gin there the green grass grows,
  Ev -- ’ry morn springs na -- tal day,
  
	
  \once \override LyricHyphen  #'minimum-distance = #0.7
  Bright -- hued ber -- ries daff the snows,
  Smil -- ing win -- ter’s frown a -- way.
  
  An -- gels oft -- en paus -- ing there,
  Doubt if E -- den were more fair,
  Beau -- ty’s home, Kil -- lar -- ney,
  Ev -- er fair Kil -- lar -- ney.
}

sopWordsIV = \lyricmode {
  \set stanza = #"4. "
  Mu -- sic there for ech -- o dwells,
  Makes each sound a har -- mo -- ny;
  Ma -- ny -- voiced the cho -- rus swells,
  Till it faints in ec -- sta -- sy.
  With the charm -- ful tints be -- low,
  Seems the heav’n a -- bove to vie,
  All rich col -- ors that we know
  Tinge the cloud -- wreaths in that sky.
  
  Wings of an -- gels so might shine,
  Glanc -- ing back soft light div -- ine,
  Beau -- ty’s home, Kil -- lar -- ney,
  Ev -- er fair Kil -- lar -- ney.
}

sopWordsV = \lyricmode {
  \set stanza = #"5. "
}

altoMusic = \relative c' {
  g'4.. g16 e4 \times 2/3 {g8[ e d]} |
  d4 e d2 |
  b'4.. a16 g4 g |
  g g fis2 |
  g4.. g16 e4 \times 2/3 {g8[ e d]} |
  
  d4 e d2 |
  g4. g8 g~ g4 e8 |
  c4.. b16 b4 s |
  e4.. e16 e4 c |
  e e d8( g4.) |
  
  g4. g8 a4 e |
  e e d2 |
  fis4 fis g g |
  g d d4. s8 |
  
  d4. e8 fis4 fis |
  d cis8.[ d16] d2 |
  d4. c8 b4 c |
  d e fis2 |
  a4.. a16 g4 fis |
  
  g d e2 |
  d4. c8 b4 c |
  d4( g) g2 |
  d4. fis8 g( b4) g8 |
  fis2 g4 s \bar"|."
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
  b4.. d16 c4 c |
  b c b2 |
  d4.. d16 d4 b |
  a e' d2 |
  b4.. d16 c4 c |
  
  b c b2 |
  d4. c8 b4. g8 |
  fis4.. g16 g4 s |
  b4.. b16 c4 a |
  b b g8( b4.) |
  
  d4. d8 c4 c |
  b8[ g] g[ b] fis2 |
  a4 c b b |
  c a g4( b8) s |
  
  d4. d8 d4 d |
  a g8.[ fis16] fis2 |
  b4. g8 g4 g |
  b b d2 |
  d4.. d16 d4 d |
  
  b8[ d] c[ b] g2 |
  b4. g8 g4 g |
  g( d') c2 |
  g4. d'8 d4. d8 |
  c2 b4 s \bar"|."
}

tenorWords = \lyricmode {
}

tenorWordsII = \lyricmode {
}

tenorWordsIII = \lyricmode {
}

bassMusic = \relative c' {
  g4.. g16 g4 g |
  g g g2 |
  g4.. g16 g4 g |
  c, cis d2 |
  g4.. g16 g4 g |
  
  g g g2 |
  g4. g8 g4. c,8 |
  d4.. g,16 g4 d'\rest |
  e4.. e16 a4 a |
  e e g8~ g4. |
  
  g4. g8 e4 c |
  e e d2\fermata |
  d4 d g g |
  e d g4~ g8 d\rest |
  
  a'4. a8 a4 a |
  a, a d2\fermata |
  g4. g8 g4 g |
  g e d2 |
  fis4.. fis16 g4 d |
  
  g g c,2 |
  g4. g8 g4 c |
  b~ \times 2/3 {b8[ g b]} c2\fermata |
  b4. d8 g4.\fermata g8 |
  d2 g4 d\rest \bar"|."
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
  title = \markup{\override #'(font-name . "Garamond Premr Pro Smdb Subh"){ \abs-fontsize #18 \smallCapsOldStyle"Killarney"}}
  composer = \markup\oldStyleNum"Michael William Balfe (1808–1870)"
  tagline = ""
}}


global = {
  \key g \major
  \time 6/8
  \dynamicUp
  %\set crescendoSpanner = #'dashed-line
  %\set midiInstrument = "recorder"
  \autoBeamOff
  \override DynamicTextSpanner #'style = #'none
}

sopMusic = \relative c' {
	\partial 4.
  d'4 c8 |
  b4 g8 a4 e8 |
  g4 e8 d4 g8 |
  a4. a |
  a d4 c8 |
  b4 g8 a4 e8 |
  g4 e8 d4 fis8 |
  
  g4. g |
  g4. g4( a8) |
  b4 c8 d4 d8 |
  e4 fis8 g4 e8 |
  d4 b8 a4 g8 |
  a4. d4 c8 |
  
  b8 g4 a4 e8 |
  g4 e8 d4 g8 |
  a4. a |
  a d4 c8 |
  b8 g4 a4 e8 |
  g4 e8 d4 e8 |
  g4. g |
  g4. \bar"|."
}
sopWords = \lyricmode {
  \set stanza = #"1. "
	Oh! the days are gone, when beau -- ty bright
  My heart’s chain wove;
  When my dream of life from morn till night,
  Was love, still love;
  New hope may bloom, and days may come
  Of mild -- er, calm -- er beam,
  But there’s noth -- ing half so sweet in life
  As love’s young dream,
  No! there’s noth -- ing half so sweet in life
  As love’s young dream.
}

sopWordsII = \lyricmode {
  \set stanza = #"2. "
  Though the bard to pur -- er fame may soar,
  When wild youth’s past;
  Though he win the wise, who frowned be -- fore,
  To smile at last;
  He’ll nev -- er meet a joy so sweet
  In all his noon of fame,
  As when first he sung to wom -- an’s ear
  His soul -- felt flame,
  And at ev -- ’ry close she blushed to hear
  The one loved name.
}

sopWordsIII = \lyricmode {
  \set stanza = #"3. "
  No! that hal -- low’d form is ne’er for -- got,
  Which first love traced;
  Still it ling -- ’ring haunts the green -- est spot
  On mem -- ’ry’s waste;
  ’Twas o -- dor fled, as soon as shed;
  ’Twas morn -- ning’s wing -- ed dream;
  ’Twas a light that ne’er can shine a -- gain
  On life’s dull stream,
  Oh! ’twas light which ne’er can shine a -- gain
  On life’s dull stream.
}

sopWordsIV = \lyricmode {
  \set stanza = #"4. "
}

sopWordsV = \lyricmode {
  \set stanza = #"5. "
}

altoMusic = \relative c' {
  \partial 4.
  b'4 a8 |
  g4 d8 e4 c8 |
  b4 c8 b4 g'8 |
  g4. g |
  fis g4 fis8 |
  g4 d8 e4 c8 |
  b4 c8 b4 c8 |
  
  d4. e8[ d c] |
  d4. b4( d8) |
  g4 a8 b4 b8 |
  c4 a8 b4 c8 |
  b4 g8 e4 e8 |
  g4( fis8) fis4 fis8 |
  
  g8 e4 e4 c8 |
  b4 c8 b4 d8 |
  g4. g |
  fis g4 fis8 |
  g8 e4 e4 c8 |
  b4 c8 b4 b8 |
  b4. c |
  d \bar"|."
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
  \partial 4.
  d4 d8 |
  d4 b8 c4 g8 |
  g4 g8 g4 b8 |
  e4. e |
  d d4 d8 |
  d4 b8 c4 g8 |
  g4 g8 g4 a8 |
  
  b4. c |
  b4. g |
  g4 g8 g4 g8 |
  g4 d'8 d4 g,8 |
  g4 g8 c4 cis8 |
  d4. d4 d8 |
  
  d8 b4 c4 g8 |
  g4 g8 g4 g8 |
  e'4. e |
  d d4 d8 |
  d8 b4 c4 g8 |
  g4 g8 g4 g8 |
  g4. c |
  b4. \bar"|."
}

tenorWords = \lyricmode {
}

tenorWordsII = \lyricmode {
}

tenorWordsIII = \lyricmode {
}

bassMusic = \relative c' {
  \partial 4.
  g4 g8 |
  g4 g8 g4 g8 |
  g,4 g8 g4 d'8 |
  c4. cis |
  d4( c?8) b4 a8 |
  g4 g8 g4 g8 |
  g4 g8 g4 d'8 |
  
  d4. c4( e8) |
  g4. g |
  g4 g8 g4 g8 |
  g4 g8 g4 g8 |
  g4 g8 a4 a8 |
  d,4. d4 d8 |
  
  g8 g4 g4 g8 |
  g,4 g8 g4 b8 |
  c4. cis |
  d4( c?8) b4 a8 |
  g8 g4 g4 g8 |
  g4 g8 g4 e'8 |
  d4. e |
  g \bar"|."
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
  title = " "
  instrument = \markup{\override #'(font-name . "Garamond Premr Pro Smdb Subh"){ \abs-fontsize #18 \smallCapsOldStyle"Love’s Young Dream"}}
  poet = \markup\oldStyleNum"Thomas Moore (1779–1852)"
  composer = \markup\oldStyleNum"Irish Air"
  tagline = ""
}}


