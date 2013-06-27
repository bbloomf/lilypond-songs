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
       (padding . -3)
       (stretchability . 100))
  ragged-last-bottom = ##f
  ragged-bottom = ##f
  two-sided = ##t
  inner-margin = 1\in
  outer-margin = 0.75\in
  top-margin = 0.26\in
  bottom-margin = 0.25\in
  first-page-number = #114
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
  \key a \major
  \time 2/4
  \tempo Andante
  \dynamicUp
  %\set crescendoSpanner = #'dashed-line
  %\set midiInstrument = "recorder"
  \autoBeamOff
  \override DynamicTextSpanner #'style = #'none
}

sopMusic = \relative c' {
	a'8 a16 b cis8 a |
  fis b4. |
  gis8 gis16 gis a8 b |
  e,4 b'\rest |
  a8 a16 b cis8 a |
  fis b4 a8 |
  
  a16[ gis] fis16[ e] e8 fis16[ gis] |
  a4 b8\rest e,^\markup\smallCapsOldStyle"Zerline" |
  a8. b16 cis8 a |
  fis b4 a8 |
  gis8. gis16 a8 b |
  e,4 b'8\rest e, |
  
  a8. b16 cis8 a |
  fis b4 a8 |
  a16[ gis] fis[ e] e8 fis16[ gis] |
  a[ cis]( e4) b8 |
  d16[ cis] b[ a] a8 cis16[ b] |
  a4 b\rest |
  
  b8^\markup\smallCapsOldStyle"Don G." gis16 e cis'8 e16[ cis] |
  b8.[ gis16] b8 b^\markup\smallCapsOldStyle"Zer." |
  ais16[ b] cis[ b] ais[ b] a[ fis] |
  gis8 gis b4\rest |
  b8^\markup\smallCapsOldStyle"Don G." gis16 e cis'8 e16[ cis] |
  
  b8.[ gis16] b8 b~^\markup\smallCapsOldStyle"Zer." |
  b b16 b ais[ b] a[ b] |
  gis[ b] b b ais[ b] a[ b] |
  gis[ b] b b ais[ b] a[ b] |
  gis8 e b'^\markup\smallCapsOldStyle"Don G." gis |
  
  d'4 b8 b\rest |
  a a16 b cis8 a |
  fis b b\rest a^\markup\smallCapsOldStyle"Zer." |
  gis8. gis16 a8 b |
  b e, b'4\rest |
  a8^\markup\smallCapsOldStyle"Don G." a16 b cis8 a |
  
  %page2
  d4 b8\rest d^\markup\smallCapsOldStyle"Zer." |
  \acciaccatura d16 gis,8. gis16 gis8 a16[ b] |
  a16 a^\markup\smallCapsOldStyle"Don G." a b cis8 a |
  d4 fis~^\markup\smallCapsOldStyle"Zer." |
  fis16[ gis,] gis gis gis32[ a b cis]( d16) b |
  
  a4 b8\rest e |
  dis16[ e] fis[ e] dis[ e] d[ b] |
  cis8 cis b\rest e~ |
  e e16 e dis[ e] d[ e] |
  
  e[ cis] cis cis bis[ cis] b[ cis] |
  cis[ a] a a gis[ a] g[ a] |
  a[ fis] fis8 b\rest d^\markup\smallCapsOldStyle"Don G." |
  gis,4 b8\rest e |
  a,4 b8\rest fis'^\markup\smallCapsOldStyle"Zer." |
  \acciaccatura e16 d4\fermata b8\rest\fermata \break
  
  \time 6/8 \tempo Allegro \partial 8
  \bar"||:"
  \repeat volta 2 {
    b8^\markup\smallCapsOldStyle"Both" |
    cis4 b8 cis4 d8 |
    b e4~ e b8 |
    cis4 b8 cis4 d8 |
    b8 e4~ e b8 |
    cis4. d8[ fis d] |
    cis4. b |
  }
  \alternative {
    {
      \partial 8*5
      a4. b4\rest
    }
    {
      a4 b8\rest a4.^\markup\smallCapsOldStyle"Don G." |
    }
  }
  d8 b4\rest e4.^\markup\smallCapsOldStyle"Zer." |
  cis8 b4\rest a4. |
  fis'8 b,4\rest e,4.^\markup\smallCapsOldStyle"Don G." |
  a8 b4\rest b\rest e8^\markup\smallCapsOldStyle"Both" |
  
  cis4 cis8 b4 b8 |
  cis4 b8\rest b4\rest e8 |
  cis4 cis8 b4 b8 |
  cis4. e |
  cis8.[ d16] cis8 b8.[ a16] b8 |
  a4 b8\rest b4\rest b8\rest \bar"|."
}
sopWords = \lyricmode {
  \set stanza = \markup\smallCapsOldStyle"Don G."
	“Nay, bid me not re -- sign, love,
  Cold -- ly the hand I press,
  Oh! say thou wilt be mine, love,
  Breathe but that one word, ‘Yes.’”
  
  “I would and yet I would not,
  I feel my heart mis -- give,
  Shouldst thou prove false, I could not
  Be -- come thy scorn and live,
  Be -- come thy scorn and live.”
  
  “Come then, oh, come then, dear -- est.”
  
  “Yet should thy fond -- ness al -- ter!”
  
  “Nay, love, in vain thou fear -- est.”
  
  “Still, still this heart will fal -- ter,
  this heart will fal -- ter,
  this heart will fal -- ter.”
  
  “Come then, come then!
  Nay bid me not re -- sign, love.”
  
  “I would, and yet I would not.”
  
  “Oh, say thou wilt be mine.”
  
  “I feel my heart mis -- give,”
  
  “Nay, love, in vain thou fear’st,”
  
  “I feel my heart mis -- give,
  Yet should thy fond -- ness al -- ter,
  Still, still this heart will fal -- ter,
  this heart will fal -- ter,
  this heart will fal -- ter,”
  
  “Oh, come, then come,”
  
  “I come.”
  
  Yes, hand and heart u -- nit -- ing,
  Each oth -- er’s vows re -- quit -- ing,
  Our joy no bounds shall know, know,
  
  Oh, come,
  
  I come, I come.
  
  Oh, come!
  
  Our joy no bounds shall know,
  Our joy no bounds shall know,
  Our joy no bounds shall know.
}

sopWordsII = \lyricmode {
}

sopWordsIII = \lyricmode {
}

sopWordsIV = \lyricmode {
  \set stanza = #"4. "
}

sopWordsV = \lyricmode {
  \set stanza = #"5. "
}

altoMusic = \relative c' {
  cis8 cis16 e e8 e |
  d fis4. |
  e8 e16 e fis8 dis |
  e4 s |
  cis8 cis16 e e8 e |
  d fis4 fis8 |
  
  e d d d |
  cis4 s8 cis |
  cis8. e16 e8 e |
  d fis4 fis8 |
  e8. e16 fis8 dis |
  e4 s8 d |
  
  cis8. e16 e8 e |
  d fis4 fis8 |
  e d d d |
  cis16[( e] a4) e8 |
  e e e gis |
  a4 s |
  
  e8 e16 e e8 e |
  e4 e8 e |
  d d d d |
  e e s4 |
  e8 e16 e e8 e |
  
  e4 e8 e~ |
  e e16 e fis8 dis |
  e e16 e fis8 dis |
  e e16 e fis8 dis |
  e e e e |
  
  gis4 gis8 s |
  e cis16 e e8 e |
  d fis s dis |
  e8. e16 fis8 dis |
  e e s4 |
  cis8 cis16 e e8 e |
  
  %page2
  a4 s8 fis |
  e8. e16 e8 e |
  e16 e cis e e8 e |
  fis4 a( |
  gis8) e16 e e8( gis16) gis |
  
  a4 s8 a |
  gis gis gis gis |
  a a s a( |
  gis) gis16 gis gis8 gis |
  
  a8 e16 e eis8 eis |
  fis fis16 fis e8 e |
  d d s fis |
  e4 s8 e |
  fis4 s8 a |
  gis4 s8 |
  
  \time 6/8
  \repeat volta 2 {
    gis8 |
    a4 gis8 a4 b8 |
    gis gis4~ gis gis8 |
    a4 gis8 a4 b8 |
    gis gis4~ gis gis8 |
    a4. a |
    a gis |
  }
  \alternative {
    {
      a s4 |
    }
    {
      a4 s8 fis4. |
    }
  }
  fis8 s4 gis4. |
  a8 s4 g4. |
  fis8 s4 d4. |
  cis8 s4 s a'8 |
  
  a4 a8 gis4 gis8 |
  a4 s8 s4 a8 |
  a4 a8 gis4 gis8 |
  a4. a |
  a8.[ b16] a8 gis8.[ fis16] gis8 |
  a4 s2 \bar"|."
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
  a8 a16 gis a8 a |
  a b4. |
  b8 b16 b b8 b |
  gis4 s |
  a8 a16 gis a8 a |
  a a4 b8 |
  
  b a16[ gis] gis8 a16[ b] |
  a4 s8 a |
  a8. gis16 a8 a |
  a b4 b8 |
  b8. b16 b8 b |
  gis4 s8 gis |
  
  a8. gis16 a8 a |
  a b4 b8 |
  b gis gis a16[ b] |
  a8( cis4) gis8 |
  a a a d |
  cis4 s |
  
  gis8 b16 gis a8 cis16[ a] |
  gis8.[ b16] gis8 gis |
  fis a fis b |
  b b s4 |
  gis8 b16 gis a8 cis16[ a] |
  
  gis8.[ b16] gis8 gis~ |
  gis gis16 gis b8 b |
  b16[ gis] gis gis b8 b |
  b16[ gis] gis gis b8 b |
  b gis gis b |
  
  b4 d8 s |
  cis a16 gis a8 a |
  a b s b |
  b8. b16 b8 a |
  gis gis s4 |
  a8 a16 gis a8 cis |
  
  %page2 (tenor)
  d4 s8 d |
  b8. b16 b8 cis16[ d] |
  cis cis a gis a8 a |
  a4 d~ |
  d8 d16 d b8~ b16 d |
  
  cis4 s8 cis |
  b b b e |
  e e s cis( b) b16 b b8 b |
  
  cis a16 a gis8 gis |
  a cis16 cis a8 a |
  fis a s d |
  b4 s8 d |
  cis4 s8 d |
  b4 s8 \bar"||:"
  
  \time 6/8
  \repeat volta 2 {
    e8 |
    e4 e8 e4 e8 |
    e b4~ b e8 |
    e4 e8 e4 e8 |
    e b4~ b e8 |
    e4. d |
    e d |
  }
  \alternative {
    {
      cis s4 |
    }
    {
      cis4 s8 d4. |
    }
  }
  a8 s4 d4. |
  cis8 s4 cis4. |
  d8 s4 gis,4. |
  a8 s4 s cis8 |
  
  e4 e8 e4 e8 |
  e4 s4. cis8 |
  e4 e8 e4 e8 e4. cis |
  e4 e8 d4 d8 |
  cis4 s2 \bar"|."
}

tenorWords = \lyricmode {
}

tenorWordsII = \lyricmode {
}

tenorWordsIII = \lyricmode {
}

bassMusic = \relative c' {
  a,8 a16 e' a8 cis, |
  d dis4. |
  e8 e16 e dis8 b |
  e4 d\rest |
  a8 a16 e' a8 cis, |
  d d4 d8 |
  
  e e e e |
  a,4 d8\rest a |
  a8. e'16 a8 cis, |
  d d4 dis8 |
  e8. e16 dis8 b |
  e4 d8\rest e |
  
  a,8. e'16 a8 cis, |
  d dis4 dis8 |
  e e e e |
  a,( a'4) e8 |
  a, cis cis e |
  a4 d,\rest |
  
  e8 e16 e e8 e |
  e4 e8 e |
  b b b b |
  e e d4\rest |
  e8 e16 e e8 e |
  
  e4 e8 e~ |
  e e16 e dis8 b |
  e e16 e dis8 b |
  e e16 e dis8 b |
  e e e e |
  e4 e8 d\rest |
  a' a,16 e' a8 cis, |
  d dis d\rest b |
  e8. e16 dis8 b |
  e e d4\rest |
  a8 a16 e' a8 gis |
  
  %page2 (bass)
  fis4 d8\rest d |
  e8. e16 e8 e |
  a16 a a, e' a8 cis, |
  d4 d( |
  e8) e16 e e8~ e16 e |
  
  a4 d,8\rest a' |
  e e e e |
  a a d,\rest a'( e) e16 e e8 e |
  
  a a,16 a cis8 cis |
  fis fis16 fis cis8 cis |
  d d d\rest d |
  e4 d8\rest e |
  fis4 d8\rest d |
  e4\fermata d8\rest \fermata \bar"||:"
  
  \time 6/8
  \repeat volta 2 {
    e8 |
    a4 e8 a4 gis8 |
    e e4~ e e8 |
    a4 e8 a4 gis8 |
    e e4~ e e8 |
    a4. fis8[ d fis] |
    e4. e |
  }
  \alternative {
    {
      a d,4\rest |
    }
    {
      a'4 d,8\rest d4. |
    }
  }
  d8 d4\rest e4. |
  a8 d,4\rest a'4. |
  d,8 d4\rest e4. |
  a,8 d4\rest d\rest a'8 |
  
  a4 a8 e4 e8 |
  a4 d,8\rest d4\rest a'8 |
  a4 a8 e4 e8 |
  a4. a |
  e4 e8 e4 e8 |
  a4 d,8\rest d4\rest d8\rest \bar"|."
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
  title = \markup{\override #'(font-name . "Garamond Premier Pro Semibold"){ \abs-fontsize #18 \smallCapsOldStyle"La ci darem la mano"}}
  composer = \markup\oldStyleNum"Wolfgang Amadeus Mozart (1756–1791)"
  arranger = \markup\oldStyleNum{"From" \italic"Don Giovanni"}
  tagline = ""
}}




