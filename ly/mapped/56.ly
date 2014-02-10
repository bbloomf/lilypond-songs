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
  first-page-number = #56
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
  \key aes \major
  \time 3/4
  \dynamicUp
  %\set crescendoSpanner = #'dashed-line
  %\set midiInstrument = "recorder"
  \autoBeamOff
  \override DynamicTextSpanner #'style = #'none
}

sopMusic = \relative c' {
	f4\p f8. g16 f8. g16 |
  aes4 aes8. bes16 aes8. bes16 |
  c4 c8. bes16 g8. aes16 |
  f4 f8. e16 c4 |
  
  f4\p f8. g16 f8. g16 |
  aes4 aes8. bes16 aes8. bes16 |
  c4 c8. bes16 g8. aes16 |
  f4 f8. f16 f4 |
  
  \repeat volta 2 {
    c'4^\mf c8. aes16 ees'8. c16 |
    bes4 bes8. aes16 g4 |
    aes aes8. f16 c'8. aes16 |
    g4 g8. e16 c4 |
    
    f4 f8. g16 f8. g16 |
    aes4 aes8. bes16 aes8. bes16 |
    c4^\markup\italic"rall." c8. bes16 g8. aes16 |
    f2 b4\rest |
  }
}
sopWords = \lyricmode {
	Gent -- ly the breez -- es blow through the for -- est;
  Birds voic -- es call -- ing; still is the night.
  Wa -- ters be -- neath them gleam -- ing in moon -- light
  Send back their an -- swers danc -- ing in light.
  
  My dear -- est heart, Oh heark -- en to me!
  Thou art a -- far, my soul cries to thee.
  No an -- swer comes from for -- est or stream -- let;
  Ech -- o but mocks at me.
}

sopWordsII = \lyricmode {
  \set stanza = #"2. "
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
  f4 f8. f16 f8. f16 |
  f4 f8. g16 f8. g16 |
  aes4 aes8. g16 e8. e16 |
  f4 f8. e16 c4 |
  
  f4 f8. f16 f8. f16 |
  f4 f8. g16 f8. g16 |
  aes4 aes8. g16 e8. e16 |
  f4 des8. des16 c4 |
  
  ees4 ees8. ees16 ees8. ees16 |
  ees4 ees8. ees16 ees4 |
  c4 c8. c16 c8. c16 |
  c4 c8. c16 c4 |
  
  f4 f8. f16 f8. f16 |
  e4 e8. e16 e8. e16 |
  e4 e8. e16 e8. e16 |
  c2 s4 |
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
  aes4 aes8. bes16 aes8. bes16 |
  c4 c8. c16 c8. c16 |
  c4 c8. c16 bes8. bes16 |
  aes4 aes8. g16 e4 |
  
  aes4 aes8. bes16 aes8. bes16 |
  c4 c8. c16 c8. c16 |
  c4 c8. c16 bes8. bes16 |
  aes4 g8. g16 aes4 |
  
  aes4 aes8. aes16 aes8. aes16 |
  g4 g8. g16 bes4 |
  aes aes8. aes16 f8. f16 |
  e4 e8. g16 g4 |
  
  aes4 aes8. bes16 aes8. bes16 |
  aes4 des8. des16 aes8. aes16 |
  aes4 g8. g16 bes8. bes16 |
  aes2 s4 |
}

tenorWords = \lyricmode {
}

tenorWordsII = \lyricmode {
}

tenorWordsIII = \lyricmode {
}

bassMusic = \relative c' {
  f,4 f8. f16 f8. f16 |
  f4 f8. f16 f8. f16 |
  e4 e8. e16 c8. c16 |
  f4 bes,8. bes16 c4 |
  
  f4 f8. f16 f8. f16 |
  f4 f8. f16 f8. f16 |
  e4 e8. e16 c8. c16 |
  f4 f8. f16 f4 |
  
  aes,4 aes8. aes16 c8. c16 |
  ees4 ees8. ees16 ees4 |
  f f8. f16 aes,8. aes16 |
  c4 c8. c16 e4 |
  
  f4 f8. f16 f8. f16 |
  des4 des8. des16 des8. des16 |
  c4 c8. c16 c8. c16 |
  f,2 d'4\rest |
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
  title = \markup{\override #'(font-name . "Garamond Premier Pro Semibold"){ \abs-fontsize #18 \smallCapsOldStyle"Night Song"}}
  composer = \markup\oldStyleNum"Swedish Folk Song"
  tagline = ""
}}


global = {
  \key aes \major
  \time 3/4
  \dynamicUp
  %\set crescendoSpanner = #'dashed-line
  %\set midiInstrument = "recorder"
  \autoBeamOff
  \override DynamicTextSpanner #'style = #'none
  \slurDashed
}

sopMusic = \relative c' {
	ees4 f g |
  aes4. bes8 c4 |
  ees,16[ aes8.] f16[ aes8.] g16[ bes8.] |
  aes2 b4\rest |
  ees,4 f g |
  aes4. bes8 c4 |
  ees,16[ aes8.] f16[ aes8.] g16[ bes8.] |
  aes2 b4\rest |
  
  c4 c c |
  ees4. ees,8 ees4 |
  c'4 c8[ ees] \times 2/3 {des[ bes g]} |
  aes4. f8 ees4 |
  ees' des8[ c] bes[ aes] |
  aes4. bes8 c4 |
  ees,16[ aes8.] f16[ aes8.] g16[ bes8.] |
  aes2 b4\rest \bar"|."
}
sopWords = \lyricmode {
  \set stanza = #"1. "
	What’s this dull town to me?
  Rob -- in’s not near.
  What was’t I wished to see,
  What wished to hear?
  
  Where’s all the joy and mirth,
  \set ignoreMelismata = ##t
  That made this town _ a heav’n on earth?
  \unset ignoreMelismata
  Oh! they’re all fled with thee,
  Rob -- in A -- dair.
}

sopWordsII = \lyricmode {
  \set stanza = #"2. "
  What made th’a -- sem -- bly shine?
  Rob -- in A -- dair.
  What made the ball so fine?
  Rob -- in was there.
  
  What, when the play was o’er,
  What made my __ heart so sore?
  Oh! it was part -- ing with
  Rob -- in A -- dair.
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
  c4 des des |
  c4. ees8 ees4 |
  ees4 des des |
  c2 s4 |
  c des des |
  c4. ees8 ees4 |
  ees des des |
  c2 s4 |
  
  ees ees ees |
  ees4. ees8 ees4 |
  ees ees8[ ges] \times 2/3 {f4( ees8)} |
  ees4. des8 c4 |
  ees ees ees |
  c4. ees8 ees4 |
  ees des des |
  c2 s4 \bar"|."
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
  aes4 aes ees |
  ees4. g8 aes4 |
  aes aes16[ f8.] ees4 |
  ees2 s4 |
  aes aes ees |
  ees4. g8 aes4 |
  aes aes16[ f8.] ees4 |
  ees2 s4 |
  
  aes aes aes |
  g4. g8 g4 |
  aes4 aes8[ aes] \times 2/3 {aes4( bes8)} |
  aes4. aes8 aes4 |
  c4 bes8[ aes] g[ aes] |
  aes4. g8 aes4 |
  aes4 aes16[ f8.] ees4 |
  ees2 s4 \bar"|."
}

tenorWords = \lyricmode {
}

tenorWordsII = \lyricmode {
}

tenorWordsIII = \lyricmode {
}

bassMusic = \relative c' {
  aes,4 des bes |
  aes4. ees'8 aes4 |
  c, des ees |
  aes,2 d4\rest |
  aes des bes |
  aes4. ees'8 aes4 |
  c, des ees |
  aes,2 d4\rest |
  
  aes aes aes |
  ees'4. ees8 ees4 |
  aes4 aes8[ c,] \times 2/3 {des4( des8)} |
  c4. des8 aes4 |
  aes aes des8[ c] |
  f4. ees8 aes,4 |
  c des ees |
  aes,2 d4\rest \bar"|."
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
  title = \markup{\override #'(font-name . "Garamond Premier Pro Semibold"){ \abs-fontsize #18 \smallCapsOldStyle"Robin Adair"}}
  composer = \markup\oldStyleNum"Scottish Folk Song"
  tagline = ""
}}


global = {
  \key c \major
  \time 4/4
  \dynamicUp
  %\set crescendoSpanner = #'dashed-line
  %\set midiInstrument = "recorder"
  \autoBeamOff
  \override DynamicTextSpanner #'style = #'none
}

sopMusic = \relative c' {
  \partial 4
	e8[ d] |
  c4. c8 c'4. c8 |
  b4 a b\rest a |
  g4 e e d8[ c] |
  \acciaccatura e8 d2 b'4\rest e,8[ d] |
  
  c4. c8 c'4. c8 |
  b4 a b\rest a |
  g4. e8 e4. d8 |
  c2 b'4\rest g |
  c4. c8 d4. d8 |
  e2 b4\rest g4 |
  
  c4. c8 d4. d8 |
  e2 e4. d8 |
  c4. b8 a4 c8[ a] |
  g4 e2 e8[ d] |
  c( c'4) e,8 e4. d8 |
  c2 b'4\rest \bar "|."
}
sopWords = \lyricmode {
  \set stanza = #"1. "
	Max -- wel -- ton braes are bon -- nie,
  Where ear -- ly fa’s the dew,
  \set ignoreMelismata = ##t
  And it’s there that An -- nie Lau -- rie,
  \unset ignoreMelismata
  Gie’d me her prom -- ise true,
  Gie’d me her prom -- ise true,
  Which ne’er for -- got will be;
  And for bon -- nie An -- nie Lau -- rie,
  I’d lay me down and dee.
}

sopWordsII = \lyricmode {
  \set stanza = #"2. "
  Her brow is like the snaw -- drift
  Her throat is like the swan,
  Her face it is the fair -- est,
  That e’er the sun shone on,
  That e’er the sun shone on;
  And dark blue is her e’e,
  And for bon -- nie An -- nie Lau -- rie,
  I’d lay me down and dee.
}

sopWordsIII = \lyricmode {
  \set stanza = #"3. "
  Like dew on~the gow -- an ly -- ing
  Is~the fa’ o’~her fair -- y __ feet,
  \set ignoreMelismata = ##t
  Like the winds in sum -- mer sigh -- ing,
  \unset ignoreMelismata
  Her voice is low and sweet,
  Her voice is low and sweet;
  She’s a’ the world to me,
  And for bon -- nie An -- nie Lau -- rie,
  I’d lay me down and dee.
  
}

sopWordsIV = \lyricmode {
  \set stanza = #"4. "
}

sopWordsV = \lyricmode {
  \set stanza = #"5. "
}

altoMusic = \relative c' {
  \partial 4
  c8[ b] |
  c4. c8 e4. g8 |
  g4 f4 s f |
  e c c b8[ c] |
  b2 s4 c8[ b] |
  
  c4. c8 e4. g8 |
  g4 f s f |
  e4. c8 c4. b8 |
  c2 s4 e4 |
  e4. e8 g4. g8 |
  g2 s4 g8[ f] |
  
  e4. g8 g4. g8 |
  g2 g4. f8 |
  e4. f8 f4 f |
  e4 c2 c8[ b] |
  c8( e4) c8 c4. b8 |
  c2 s4 \bar"|."
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
  g8[ f] |
  e4. e8 g4. g8 |
  c4 c s c |
  c g g f8[ e] |
  g2 s4 g8[ f] |
  
  e4. e8 g4. g8 |
  c4 c s c |
  c4. g8 g4. f8 |
  e2 s4 c' |
  g4. g8 b4. b8 |
  c2 s4 d |
  
  c4. c8 b4. b8 |
  c2 c4. b8 |
  c4. c8 c4 a |
  c4 g2 g8[ f] |
  g4. g8 g4. f8 |
  e2 s4 \bar"|."
}

tenorWords = \lyricmode {
}

tenorWordsII = \lyricmode {
}

tenorWordsIII = \lyricmode {
}

bassMusic = \relative c' {
  \partial 4
  c,4 |
  c4. c8 c4. e8 |
  f4 f d\rest f |
  g g, g g |
  g2 d'4\rest g,8[ g] |
  
  c4. c8 c4. e8 |
  f4 f d\rest f4 |
  g4. g,8 g4. g8 |
  c2 d4\rest c |
  c4. c8 g'4. g8 |
  c,2 d4\rest g4 |
  
  c,4. e8 g4. g8 |
  c,2 g'4. g8 |
  a4. f8 f4 f |
  c4 c2 c4 |
  e8( c4) g8 g4. g8 |
  c2 d4\rest \bar"|."
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
  title = \markup{\override #'(font-name . "Garamond Premier Pro Semibold"){ \abs-fontsize #18 \smallCapsOldStyle"Annie Laurie"}}
  composer = \markup\oldStyleNum"Lady John Scott (1810–1900)"
  poet = \markup\oldStyleNum"William Douglas (c. 1672–1748)"
  tagline = ""
}}


