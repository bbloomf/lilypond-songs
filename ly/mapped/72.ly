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
  score-markup-spacing =
    #'((basic-distance . 0)
       (minimum-distance . 0)
       (padding . -14)
       (stretchability . 70))
  top-system-spacing =
    #'((basic-distance . 0)
       (minimum-distance . 0)
       (padding . -1)
       (stretchability . 0))
  ragged-last-bottom = ##f
  ragged-bottom = ##f
  two-sided = ##t
  inner-margin = 1\in
  outer-margin = 0.75\in
  top-margin = 0.26\in
  bottom-margin = 0.25\in
  first-page-number = #72
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
  \key bes \major
  \time 4/4
  \dynamicUp
  %\set crescendoSpanner = #'dashed-line
  %\set midiInstrument = "recorder"
  \autoBeamOff
  \override DynamicTextSpanner #'style = #'none
}

sopMusic = \relative c' {
	\partial 4
  f4\mf |
  f4. d8 d4 f |
  f4. c8 c4 d |
  ees f g a |
  f2. \bar"" f4 |
  
  f4. d8 d4 f |
  f4. c8 c4 c' |
  b c d g, |
  c2. \bar"" f,4\f |
  
  d'4. d8 c4 bes |
  bes4. a8 a4 bes |
  c a g f |
  bes2. \bar"" bes4 |
  
  bes4. g8 g4 bes |
  bes4. f8 f4 f |
  g^\markup\italic"rall." bes f c' |
  bes2. \bar"|."
}
sopWords = \lyricmode {
  \set stanza = #"1. "
	\set associatedVoice = "altos"
  Oh beau -- ti -- ful for spa -- cious skies,
  For am -- ber waves of grain, __
  For pur -- ple moun -- tain maj -- es -- ties
  A -- bove the fruit -- ed plain! __
  A -- mer -- i -- ca! A -- mer -- i -- ca!
  God shed His grace on thee, __
  And crown thy good with bro -- ther -- hood
  From sea to shin -- ing sea!
}

sopWordsII = \lyricmode {
  \set stanza = #"2. "
  \set associatedVoice = "altos"
  O beau -- ti -- ful for pil -- grim feet,
  Whose stern im -- pas -- sion’d stress, __
  A thor -- ough -- fare for free -- dom beat
  A -- cross the wil -- der -- ness! __
  A -- mer -- i -- ca! A -- mer -- i -- ca!
  God mend thine ev -- ’ry flaw, __
  Con -- firm thy soul in self con -- trol,
  Thy lib -- er -- ty in law!
}

sopWordsIII = \lyricmode {
  \set stanza = #"3. "
}

sopWordsIV = \lyricmode {
  \set stanza = #"4. "
}

sopWordsV = \lyricmode {
  \set stanza = #"5. "
}

altoMusic = \relative c' {
  \partial 4
  f4 |
  d4. bes8 bes4 d |
  c4. a8 a4 a |
  c c ees ees |
  d2( ees4) c |
  
  d4. bes8 bes4 d |
  c4. c8 c4 f |
  f f e e |
  f( e ees) f |
  
  f4. f8 f4 d |
  ees4. f8 f4 f |
  f f ees ees |
  d2( ees4) f |
  
  ees4. ees8 ees4 ees |
  d4. d8 d4 f |
  g bes f ees |
  d2. \bar"|."
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
  \partial 4
  bes4 |
  bes4. f8 f4 bes |
  a4. f8 f4 f |
  a a c c |
  bes( gis a) a |
  
  bes4. f8 f4 bes |
  a4. a8 a4 a |
  a a bes bes |
  a( bes c)
  f, |
  
  bes4. bes8 bes4 bes |
  c4. c8 c4 bes |
  a a bes c |
  bes( f g) aes |
  
  g4. g8 g4 g |
  f4. bes8 bes4 f |
  g bes f a |
  bes2. \bar"|."
}

tenorWords = \lyricmode {
}

tenorWordsII = \lyricmode {
}

tenorWordsIII = \lyricmode {
}

bassMusic = \relative c' {
  \partial 4
  bes,4 |
  bes4. bes8 bes4 f |
  c'4. c8 c4 f, |
  c' c f f, |
  bes( b c) f, |
  
  bes?4. bes8 bes4 g |
  c4. c8 c4 c |
  c c c g' |
  f( g a) f |
  
  f4. f8 d4 f |
  f4. c8 c4 d |
  ees f g a |
  bes( bes,2) bes4 |
  
  ees4. ees8 ees4 c |
  f4. f8 f4 f |
  g bes f f, |
  bes2. \bar"|."
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
  title = \markup{\override #'(font-name . "Garamond Premr Pro Smdb Subh"){ \abs-fontsize #18 \smallCapsOldStyle"America the Beautiful"}}
  composer = \markup\oldStyleNum"Samuel Augustus Ward (1847–1903)"
  poet = \markup\oldStyleNum"Katherine Lee Bates (1859–1929)"
  tagline = ""
}}

global = {
  \key aes \major
  \time 4/4
  \dynamicUp
  %\set crescendoSpanner = #'dashed-line
  %\set midiInstrument = "recorder"
  \autoBeamOff
  \override DynamicTextSpanner #'style = #'none
}

sopMusic = \relative c' {
	\partial 4
  ees4 |
  c' c c c |
  des2( c) |
  bes8 bes bes bes c4 bes |
  aes2. ees4 |
  c' c c c |
  
  des2( c) |
  bes8 bes bes bes c4 bes |
  aes2. b4\rest |
  \repeat volta 2 {
    f aes des f, |
    ees aes c2 |
    c4 bes g ees |
    
    bes' aes ees2 |
    f4 aes des f, |
    ees aes c c8 c |
    c4 bes g ees |
    aes2.
  }
}
sopWords = \lyricmode {
  \set stanza = #"1. "
	There’s mu -- sic in the air,
  When the in -- fant morn is nigh,
  And faint its blush is seen
  On the bright and laugh -- ing sky.
  Ma -- ny~a harp’s ec -- stat -- ic sound
  Thrills us with its joy pro -- found,
  While we list, en -- chant -- ed there,
  To the mu -- sic in the air.
}

sopWordsII = \lyricmode {
  \set stanza = #"2. "
  There’s mu -- sic in the air,
  When the noon -- tide’s sul -- try beam
  Re -- flects a gold -- en light
  On the dis -- tant moun -- tain stream.
  When be -- neath some grate -- ful shade
  Sor -- row’s ach -- ing head is laid,
  Sweet -- ly to the spi -- rit there
  Comes the mu -- sic in the air.
}

sopWordsIII = \lyricmode {
  \set stanza = #"3. "
  There’s mu -- sic in the air,
  When the twi -- light’s gen -- tle sigh
  Is lost on eve -- ning’s breast,
  As its pen -- sive beau -- ties die:
  Then, O, then, the loved ones gone
  Wake the pure, ce -- les -- tial song;
  An -- gel voi -- ces greet us there
  With the mu -- sic in the air.
}

sopWordsIV = \lyricmode {
  \set stanza = #"4. "
}

sopWordsV = \lyricmode {
  \set stanza = #"5. "
}

altoMusic = \relative c' {
  \partial 4
  c4 |
  ees ees ees ees |
  f2( ees) |
  des8 des des des ees4 des |
  c2. c4 |
  ees ees ees ees |
  
  f2( ees) |
  des8 des des des ees4 des |
  c2. s4 |
  des f f des |
  c c ees2 |
  ees4 des des des |
  
  c c c2 |
  des4 f f des |
  c c ees ees8 ees |
  ees4 des des des |
  c2.
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
  \partial 4
  aes4 |
  aes aes aes aes |
  aes1 |
  g8 g g g g4 g |
  aes2. aes4 |
  aes aes aes aes |
  aes1 |
  g8 g g g g4 g |
  aes2. s4 |
  aes aes aes aes |
  aes aes aes2 |
  g4 g g g |
  
  aes aes aes2 |
  aes4 aes aes aes |
  aes aes aes aes8 aes |
  g4 g bes g |
  aes2.
}

tenorWords = \lyricmode {
}

tenorWordsII = \lyricmode {
}

tenorWordsIII = \lyricmode {
}

bassMusic = \relative c' {
  \partial 4
  aes,4 |
  aes aes aes aes |
  aes1 |
  ees'8 ees ees ees ees4 ees |
  aes,2. aes4 |
  aes aes aes aes |
  
  aes1 |
  ees'8 ees ees ees ees4 ees |
  aes,2. d4\rest |
  des des des des |
  aes aes aes2 |
  ees'4 ees ees ees |
  
  aes, aes aes2 |
  des4 des des des |
  aes aes aes aes8 aes |
  ees'4 ees ees ees |
  aes,2.
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
  title = \markup{\override #'(font-name . "Garamond Premr Pro Smdb Subh"){ \abs-fontsize #18 \smallCapsOldStyle"There’s Music in the Air"}}
  composer = \markup\oldStyleNum"George Frederick Root (1820–1895)"
  tagline = ""
}}


global = {
  \key ees \major
  \time 6/8
  \dynamicUp
  %\set crescendoSpanner = #'dashed-line
  %\set midiInstrument = "recorder"
  \autoBeamOff
  \override DynamicTextSpanner #'style = #'none
}

sopMusic = \relative c' {
	\partial 8
  ees8 |
  g4 f8 g4 aes8 |
  bes4. c8 << bes4 {s8 \teeny g8} >> \normalsize |
  \slurDashed
  ees8( d) ees aes4 g8 |
  \slurSolid
  f4.~ f4 \bar"" d8 |
  
  %page2
  ees4 f8 g4 aes8 |
  bes4 c8 bes4 bes16[ c] |
  d4 bes8 a4 c8 |
  bes4.~ bes4 \bar"" bes8 |
  
  bes4 aes8 aes4 aes8 |
  g4 bes8 ees4 d8 |
  c4 bes8 aes4 g8 |
  aes4.~ aes4 \bar"" c8 |
  
  c4 bes8 aes4 g8 |
  g4 f8 ees4 es8 |
  g4 f8 ees4 d8 |
  ees4.~ ees4 \bar"|."
}
sopWords = \lyricmode {
  \set stanza = #"1. "
	’Twas when the hay was mown, Mag -- gie, ""
  \set ignoreMelismata = ##t
  In the long \unset ignoreMelismata years a -- go, __
  And while the wes -- tern sky was rich
  With sun -- set’s ros -- y glow, __
  Then hand in hand close linked we passed
  The dew -- y ricks be -- tween,
  When I was one and twen -- ty, Mag,
  And you were sev -- en -- teen. __
}

sopWordsII = \lyricmode {
  \set stanza = #"2. "
  Your voice was low and sweet, Mag -- gie,
  Your wav -- y hair was brown,
  Your cheek was like the wild red rose
  That show’rs its pet -- als down; __
  Your eyes were like the blue speed -- well
  With dew -- y mois -- ture sheen, __
  %When I was one and twen -- ty, Mag,
  %And you were sev -- en -- teen. __
}

sopWordsIII = \lyricmode {
  \set stanza = #"3. "
  The years have come and gone, Mag -- gie,
  With sun -- shine and with shade, __
  And sil -- vered is the silk -- en hair
  That o’er your shoul -- ders strayed,
  In ma -- ny~a soft and way -- ward tress,
  The fair -- est ev -- er seen, __
  %When I was one and twen -- ty, Mag,
  %And you were sev -- en -- teen. __
}

sopWordsIV = \lyricmode {
  \set stanza = #"4. "
  Though gen -- tly chang -- ing time, Mag -- gie,
  Has touched you in his flight, __
  Your voice has still the old sweet tone,
  Your eyes the old love light, __
  And years can nev -- er, nev -- er change,
  The heart you gave, I ween, __
  %When I was one and twen -- ty, Mag,
  %And you were sev -- en -- teen. __
}

sopWordsV = \lyricmode {
  \set stanza = #"5. "
}

altoMusic = \relative c' {
  ees8 |
  
  ees4 d8 ees4 f8 |
  g4. ees8 << g4 {s8 \teeny ees} >> \normalsize |
  \tieDashed
  c8~ c c ees4 ees8 |
  \tieSolid
  d4.~ d4 d8 |
  
  %page2
  bes4 bes8 ees4 f8 |
  g4 g8 g4 g8 |
  bes4 bes8 ees,4 ees8 |
  d4.~ d4 d8 |
  
  d4 d8 d4 bes8 |
  ees4 g8 g4 g8 |
  ees4 ees8 f4 e8 |
  f4.~ f4 aes8 |
  
  aes4 f8 d4 ees8 |
  d4 d8 c4 c8 |
  c4 c8 bes4 bes8 |
  bes4.~ bes4 \bar"|."
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
  ees,8 |
  bes'4 bes8 bes4 bes8 |
  bes4. aes8 << bes4 { s8 \teeny bes8 } >> \normalsize |
  \tieDashed
  g8~ g g c4 bes8 |
  \tieSolid
  bes4.~ bes4 aes8 |
  
  %page2
  g4 g8 bes4 bes8 |
  bes4 ees8 ees4 ees8 |
  d4 d8 c4 a8 |
  bes4.~ bes4 bes8 |
  
  f4 f8 bes4 bes8 |
  bes4 bes8 bes4 b8 |
  c4 bes?8 c4 c8 |
  c4.~ c4 ees8 |
  
  bes4 aes8 f4 bes8 |
  b4 b8 g4 g8 |
  ees4 ees8 f4 f8 |
  g4.~ g4 \bar"|."
}

tenorWords = \lyricmode {
}

tenorWordsII = \lyricmode {
}

tenorWordsIII = \lyricmode {
}

bassMusic = \relative c' {
  ees,8 |
  ees4 bes8 ees4 d8 |
  ees4. ees8 << ees4 { s8 \teeny ees8 } >> \normalsize |
  \tieDashed
  c8~ c c aes4 g8 |
  \tieSolid
  bes4.~ bes4 bes8 |
  
  %page2
  ees4 ees8 ees4 ees8 |
  ees4 ees8 ees4 ees8 |
  f4 f8 f,4 f8 |
  bes4.~ bes4 bes8 |
  
  bes4 bes8 bes4 d8 |
  ees4 ees8 ees4 g8 |
  aes4 g8 f4 c8 |
  f4.( ees4) ees8 |
  
  d4 d8 bes4 ees8 |
  g4 g8 c,4 c8 |
  aes4 aes8 bes4 bes8 |
  ees4.~ ees4 \bar"|."
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
  title = \markup{\override #'(font-name . "Garamond Premier Pro Semibold"){ \abs-fontsize #18 \smallCapsOldStyle"The Old Time"}}
  composer = \markup\oldStyleNum"J. R. Thomas, 1873"
  tagline = ""
}}


      